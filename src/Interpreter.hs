module Interpreter where

import InterpretTypes
import ExprUtils
import AbsGramatyka
import Arrays
import Functions
import System.IO
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map

evaluateExpr :: Expr Pos -> ProgramMonad ExpValue
evaluateExpr e = case e of
                    ELitInt _ i -> return (IEV i)
                    ELitTrue _ -> return (BEV True)
                    ELitFalse _ -> return (BEV False)
                    EVar _ id -> getVarValue id;
                    EAdd _ exp1 op exp2 -> addOpEvalM op (evaluateExpr exp1) (evaluateExpr exp2)
                    EMul pos exp1 op exp2 -> mulOpEvalM pos op (evaluateExpr exp1) (evaluateExpr exp2)
                    EString _ s -> return (SEV s);
                    Not _ exp -> do {
                                b <- boolExpValueM $ evaluateExpr exp;
                                return (BEV $ not b);
                               }
                    EAnd _ exp1 exp2 -> do {
                                        b1 <- boolExpValueM $ evaluateExpr exp1;
                                        b2 <- boolExpValueM $ evaluateExpr exp2;
                                        return (BEV $ and [b1, b2]);
                                      }
                    EOr _ exp1 exp2 -> do {
                                        b1 <- boolExpValueM $ evaluateExpr exp1;
                                        b2 <- boolExpValueM $ evaluateExpr exp2;
                                        return (BEV $ or [b1, b2]);
                                      }
                    ERel _ exp1 op exp2 -> do {
                                            b <- relExpValueM op (evaluateExpr exp1) (evaluateExpr exp2);
                                            return $ BEV b;
                                         }
                    Neg _ exp -> do {
                                val <- evaluateExpr exp;
                                case val of
                                    IEV i -> return $ IEV (-i);
                                    _ -> throwError "Dont know how to negate!";
                               }
                    EApp _ id exps -> do {
                                        val <- getVarValue id;
                                        case val of
                                            FEV (Function (b, f, e)) -> evalFunction b exps f e
                                            _ -> throwError $ "Variable is not a function" ++ (show id)
                                    }
                    EArrAcc pos id (exprs) -> do {
                                            indicesVal <- liftMList $ map(\(ArrInd _ expr)-> evaluateExpr expr)  exprs;
                                            indices <- liftMList $ map(\val -> evalExpValueToInt val) indicesVal;
                                            loc <- getVarLoc id;
                                            getArrValue pos indices loc;
                                        }
                    AnF pos type_ args block -> let fDef = (functionDefinition args block) in do {
                                            env <- ask;
                                            return (FEV $ Function (getReferenceMarks args ,fDef, env));
                    }

varAssignment :: ExpValue -> Ident -> ProgramMonad ()
varAssignment eval id = do {
                        loc <- getVarLoc id;
                        modify(modifyState loc eval);
                        }

getFunArgs :: [Bool] -> [Expr Pos] -> ProgramMonad [FuncArg]
getFunArgs [] _ = return [];
getFunArgs (True : bools) (EVar p id:exps) = do {
                tailF <- getFunArgs bools exps;
                loc <- getVarLoc id;
                return (L loc:tailF)
            }
getFunArgs (True : bools) (EArrAcc p id indicesE:exps) = do {
                             tailF <- getFunArgs bools exps;
                             loc <- getVarLoc id;
                             indicesVal <- liftMList $ map(\(ArrInd _ expr)-> evaluateExpr expr) indicesE;
                             indices <- liftMList $ map(\val -> evalExpValueToInt val) indicesVal;
                             slice_loc <- getArrValueLoc p    indices        loc;
                             return (L slice_loc:tailF)
                         }
getFunArgs (b:bools) (e:exps) = do {
                tailF <- getFunArgs bools exps;
                eval <- evaluateExpr e;
                return (E eval :tailF);
            }

evalFunction :: [Bool] -> [Expr Pos] -> ([FuncArg] -> ProgramMonad ExpValue) -> Env -> ProgramMonad ExpValue
evalFunction b exps f env = do {
                            funcArgs <- getFunArgs b exps;
                            retVal <- local(\e -> env) (f funcArgs);
                            return retVal;
                        }


modifyEnvForFunctionCall :: [Loc] -> [Arg Pos] -> [FuncArg] -> ProgramMonad (Env -> Env)
modifyEnvForFunctionCall _ [] _ = return (id)
modifyEnvForFunctionCall (l:ls) (arg:xs) (L loc:fs) = let id = getArgId arg in do {
                                                        envF <- modifyEnvForFunctionCall ls xs fs;
                                                        return (\e -> envF(Map.insert id l e));
                                                     }

modifyEnvForFunctionCall (l:ls) (arg:xs) (E eval:fs) = let id = getArgId arg in do {
                                                        envF <- modifyEnvForFunctionCall ls xs fs;
                                                        modify (modifyState l eval);
                                                        return (\e -> envF(Map.insert id l e));
                                                     }
modifyEnvForFunctionCall _ _ _  = undefined

functionDefinition :: [Arg Pos] -> Block Pos -> [FuncArg] -> ProgramMonad ExpValue
functionDefinition args block funa = do {
                                        locs <- allocForArgs args;
                                        envF <- modifyEnvForFunctionCall (zipLocs locs funa) args funa;
                                        retVal <- local (envF) $ interpretStmts [BStmt Nothing block];
                                        return retVal;
                                       }

interpretStmts :: [Stmt Pos] -> ProgramMonad ExpValue
interpretStmts [] = return NoReturnEV;
interpretStmts (Empty _ :xs) = interpretStmts xs;
interpretStmts [BStmt _ (Block _ s)] = interpretStmts s;
interpretStmts (BStmt _ (Block _ s):xs) = do {
                                        ret <- interpretStmts s;
                                        case ret of
                                            NoReturnEV -> interpretStmts xs;
                                            _ -> return ret;
                                    }
interpretStmts (Print _ exp:xs) = do {
                                    s <- showExpValueM $ evaluateExpr exp;
                                    liftIO $ putStrLn $ s;
                                    interpretStmts xs;
                                }
interpretStmts (Cond _ exp s:xs) = do {
                                    eval <- boolExpValueM $ evaluateExpr exp;
                                    if (eval) then interpretStmts (s:xs)
                                    else interpretStmts (xs);
                                }
interpretStmts (CondElse _ exp s1 s2:xs) = do {
                                    eval <- boolExpValueM $ evaluateExpr exp;
                                    if (eval) then interpretStmts (s1:xs)
                                    else interpretStmts (s2:xs);
                                }
interpretStmts (While pos exp s:xs) = do {
                                    eval <- boolExpValueM $ evaluateExpr exp;
                                    if (eval) then do
                                            val <- interpretStmts [s];
                                            case val of
                                                BreakEV -> interpretStmts (xs)
                                                _ -> interpretStmts ((While pos exp s):xs)
                                    else interpretStmts (xs);
                                }
interpretStmts (Decl _ (Arr _ _ sizes) (NoInit _ id) : xs) = do {
                                                        arrloc <- alloc;
                                                        allocArray arrloc (map(\(ArrSize _ i) -> i) sizes);
                                                        local (Map.insert id arrloc) $ interpretStmts xs;
                                                    }
interpretStmts (Decl _ t (NoInit _ id) : xs) = do {
                                            newloc <- alloc;
                                            local(Map.insert id newloc) $ interpretStmts xs;
                                          }
interpretStmts (Decl _ t (Init _ id exp) : xs) = do {
                                            newloc <- alloc;
                                            eval <- evaluateExpr exp;
                                            modify (modifyState newloc eval);
                                            local(Map.insert id newloc) $ interpretStmts xs;
                                          }
interpretStmts (Ass pos id (EArrAcc pos2 id2 indicesE) : xs) = do {
                             loc <- getVarLoc id2;
                             indicesVal <- liftMList $ map(\(ArrInd _ expr)-> evaluateExpr expr) indicesE;
                             indices <- liftMList $ map(\val -> evalExpValueToInt val) indicesVal;
                             slice_loc <- getArrValueLoc pos2 indices loc;
                             eval <- getLocValue slice_loc;
                             varAssignment eval id;
                             interpretStmts xs;
}
interpretStmts (Ass _ id exp : xs) = do {
                                    eval <- evaluateExpr exp;
                                    varAssignment eval id;
                                    interpretStmts xs;
                                }
interpretStmts (Incr pos id : xs) = interpretStmts (Ass pos id (EAdd pos (EVar pos id) (Plus pos) (ELitInt pos 1)) :xs)
interpretStmts (Decr pos id : xs) = interpretStmts (Ass pos id (EAdd pos (EVar pos id) (Minus pos) (ELitInt pos 1)) :xs)
interpretStmts (VRet _ :xs) = return VoidEV;
interpretStmts (Ret _ exp:xs) = do {
                                  val <- evaluateExpr exp;
                                  return val;
                              }
interpretStmts ((FnDef _ _ id args b):xs) = let fDef = (functionDefinition args b) in do {
                                            newloc <- alloc;
                                            env <- ask;
                                            modify (modifyState newloc (FEV $ Function (getReferenceMarks args ,fDef, (Map.insert id newloc env))));
                                            local(Map.insert id newloc) $ interpretStmts xs;
                                          }
interpretStmts ((SExp _ e):xs) = do {
                                    evaluateExpr e;
                                    interpretStmts xs;
                                }
interpretStmts (Continue _ :xs) = return ContinueEV;
interpretStmts (Break _ :xs) = return BreakEV;
interpretStmts (ArrInit pos id ixs exp:xs) = do {
                                            loc <- getVarLoc id;
                                            indicesVal <- liftMList $ map(\(ArrInd _ expr)-> evaluateExpr expr) ixs;
                                            indices <- liftMList $ map(\val -> evalExpValueToInt val) indicesVal;
                                            value_loc <- getArrValueLoc pos indices loc;
                                            val <- evaluateExpr exp;
                                            modify (modifyState value_loc val);
                                            interpretStmts xs;
                                        }

interpret :: Program Pos -> IO ()
interpret (Program _ stmts) = do {
                                res <- (runStateT $ (runReaderT $ runExceptT  $ interpretStmts stmts) Map.empty) (0, Map.empty);
                                case fst res of
                                    Left errMsg -> hPutStrLn stderr errMsg
                                    Right _ -> return ();
                              }
