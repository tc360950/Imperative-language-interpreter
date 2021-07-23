module ExprUtils where 
import InterpretTypes
import AbsGramatyka
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe                   
                    
evalExpValueToInt :: ExpValue -> ProgramMonad Integer 
evalExpValueToInt (IEV i) = return i;
evalExpValueToInt _ = undefined
                       
addOpEval :: AddOp a -> ExpValue -> ExpValue -> ProgramMonad ExpValue
addOpEval op (IEV i) (IEV j) = case op of 
                                Plus _ -> return  $ IEV (i + j);
                                Minus _ -> return $ IEV (i - j);
addOpEval _ _ _ = undefined

addOpEvalM :: AddOp a -> ProgramMonad ExpValue -> ProgramMonad ExpValue -> ProgramMonad ExpValue
addOpEvalM op e1 e2 = do {
                        v1 <- e1;
                        v2 <- e2;
                        addOpEval op v1 v2;
                    }

mulOpEval :: Pos -> MulOp a -> ExpValue -> ExpValue -> ProgramMonad ExpValue
mulOpEval pos op (IEV i) (IEV j) = case op of
                                    Times _ -> return  $ IEV (i*j);
                                    Div _ -> if j == 0 then
                                                throwError $ "Runtime error at " ++ (show $ fromMaybe (0,0) pos) ++ " division by zero!";
                                             else
                                                return $ IEV (i `div` j);
                                    Mod _ -> return $ IEV (i `mod` j);
mulOpEval _ _ _ _ = undefined
                  
mulOpEvalM :: Pos -> MulOp a -> ProgramMonad ExpValue -> ProgramMonad ExpValue -> ProgramMonad ExpValue
mulOpEvalM pos op e1 e2 = do {
                        v1 <- e1;
                        v2 <- e2;
                        mulOpEval pos op v1 v2;
                     }  

showExpValueM :: ProgramMonad ExpValue -> ProgramMonad String 
showExpValueM me = do {
                    e <- me;
                    case e of 
                        IEV i -> return $ Prelude.show i;
                        SEV s -> return s;
                        BEV b -> return $ Prelude.show b;
                        _ -> undefined
                }

                    
boolExpValueM :: ProgramMonad ExpValue -> ProgramMonad Bool 
boolExpValueM mb = do {
                        b <- mb;
                        case b of 
                            BEV bool -> return bool;
                            _ -> undefined
                  }
                  
relExpValueM :: RelOp a -> ProgramMonad ExpValue -> ProgramMonad ExpValue -> ProgramMonad Bool 
relExpValueM op e1 e2 = do {
                            ev1 <- e1;
                            ev2 <- e2;
                            relExpValue op ev1 ev2;
                        }

relExpValue :: RelOp a -> ExpValue -> ExpValue -> ProgramMonad Bool 
relExpValue op (IEV i) (IEV j) = case op of 
                                    LTH _ -> return (i < j);
                                    LE _ -> return (i <= j);
                                    GTH _ ->  return (i > j);
                                    GE _ -> return (i >= j);
                                    EQU _ -> return (i == j);
                                    NE _ -> return (i /= j);
relExpValue op (BEV i) (BEV j) = case op of 
                                    EQU _ -> return (i == j);
                                    NE _ -> return (i /= j);
                                    _ -> undefined
relExpValue op (SEV i) (SEV j) = case op of 
                                    EQU _ -> return (i == j);
                                    NE _ -> return (i /= j);
                                    _ -> undefined
relExpValue _ _ _ = undefined
