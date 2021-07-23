module TypeValidator where
import AbsGramatyka
import InterpretTypes
import Functions
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import System.IO
import System.Environment

-- Integer - poziom zagniezdzenia, Type - oczekiwany typ return'a (return programu to Void), Mapa - id zmiennej do (poziom zagniezdzenia, na ktorym zdefiniowano zmienna, typ, czy zainicjalizowana)
type TypeEnv = (Integer, Type () , Map Ident (Integer, Type Pos, Bool))

type TypeMonad a = ExceptT String (ReaderT TypeEnv IO ) a

checkIfArrByRef :: [Arg Pos] -> Bool
checkIfArrByRef [] = True
checkIfArrByRef (RefArg _ _  _:args) = checkIfArrByRef args
checkIfArrByRef (Arg _ (Arr _ _ _) _:args) = False;
checkIfArrByRef (_ :args) = checkIfArrByRef args;

mapArgsToArgT :: [Arg Pos] -> [ArgT Pos]
mapArgsToArgT = map (\a -> case a of {Arg p t _ -> ValArgT p t; RefArg p t _ -> RefArgT p t })

getTypePos :: Type Pos -> Pos;
getTypePos t = case t of
                Int pos -> pos
                Str pos -> pos
                Bool pos -> pos
                Void pos -> pos
                Fun pos _ _ -> pos
                Arr pos _ _ -> pos

differentTypes :: Type a -> Type b -> Bool
differentTypes e f = not $ sameTypes e f

allDifferent :: (Eq a) => [a] -> Bool
allDifferent list = case list of
    []      -> True
    (x:xs)  -> x `notElem` xs && allDifferent xs


sameArgT :: ArgT a -> ArgT b -> Bool;
sameArgT (ValArgT _ t1) (ValArgT _ t2) = sameTypes t1 t2;
sameArgT (RefArgT _ t1) (RefArgT _ t2) = sameTypes t1 t2;
sameArgT _ _ = False;

sameTypes :: Type a -> Type b -> Bool
sameTypes (Int _) (Int _) = True;
sameTypes (Str _ ) (Str _) = True;
sameTypes (Bool _) (Bool _) = True;
sameTypes (Void _) (Void _) = True
sameTypes (Fun _ t argsT) (Fun _ t2 argsT2) = if length(argsT) /= length(argsT2) then False
                                              else and [sameTypes t t2, and $ zipWith (\a -> \b -> sameArgT a b) argsT argsT2]
sameTypes (Arr _  t sizes1) (Arr _ t2 sizes2) = if length(sizes1) /= length(sizes2) then False
                                                else and [sameTypes t t2, and $ zipWith (\(ArrSize _ i) -> \(ArrSize _ j) -> i == j) sizes1 sizes2]
sameTypes _ _ = False

ofArrayType :: Type a -> Bool
ofArrayType (Arr _ _ _) = True
ofArrayType _ = False

hasVoidArgs :: [Arg a] -> Bool
hasVoidArgs args =  not $ null (filter (sameTypes (Void ())) (map (\a -> case a of {Arg _ t _  -> t; RefArg _ t _ -> t}) args))


showT :: Type a -> String
showT t = case t of
            Int _ -> "integer"
            Str _ -> "string"
            Bool _ -> "boolean"
            Void _ -> "void"
            Fun _ t args -> "\nfunction returning " ++ (showT t) ++ " with args: \n    " ++  ( intercalate "\n" $ map (\arg -> case arg of {ValArgT _ t_ -> "    " ++ (showT t_); RefArgT _ t_ -> "    &" ++ (showT t_)}) args)
            Arr _ t s -> (show $ length s) ++ " dimensional array of " ++ (showT t)

showE :: Expr Pos -> String
showE e  = show $ fmap (\c -> ()) e

checkIfInt :: [Expr Pos] -> TypeMonad ()
checkIfInt [] = return ();
checkIfInt exps = do {
                      types <- liftMList $ map (\e -> getExpressionType e) exps;
                      if (null $ filter (\t -> not $ sameTypes (Int ()) t) types) then
                        return ();
                      else
                            throwError "";
                  }

checkFunctionArgs :: [ArgT Pos] -> [Expr Pos] -> TypeMonad ();
checkFunctionArgs [] _ = return ();
checkFunctionArgs (arg:args) (exp:exps) = let t = (case arg of {ValArgT _ t -> t; RefArgT _ t -> t}) in do {
                                                         case arg of
                                                             RefArgT _ _ ->
                                                                    case exp of
                                                                          EVar p id ->
                                                                                do {
                                                                                    type_ <- getVariableType p id;
                                                                                    if differentTypes t type_ then
                                                                                        throwError $ "Error at " ++ (show $ fromMaybe (0,0) $ getExpPos exp) ++ " expression is of type \n" ++ (showT type_)
                                                                                                                                      ++ " but should be of type \n" ++ (showT t);
                                                                                    else
                                                                                        checkFunctionArgs args exps;
                                                                                 }
                                                                          EArrAcc p id arr_indices -> do {
                                                                                   catchError  (checkIfInt $ map (\(ArrInd _ exp ) -> exp) arr_indices) (\e -> throwError $ "Array index must be an integer\n" ++ e);
                                                                                   type_ <- getArraySliceType p id arr_indices;
                                                                                   if differentTypes t type_ then
                                                                                       throwError $ "Error at " ++ (show $ fromMaybe (0,0) $ getExpPos exp) ++ " expression is of type \n" ++ (showT type_)
                                                                                                                                     ++ " but should be of type \n" ++ (showT t);
                                                                                   else
                                                                                       checkFunctionArgs args exps;
                                                                           }
                                                                          _ -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) $ getExpPos exp) ++ " expression is not a variable!";
                                                             ValArgT _ _ -> do {
                                                                           type_ <- getExpressionType exp;
                                                                           if differentTypes t type_ then
                                                                              throwError $ "Error at " ++ (show $ fromMaybe (0,0) $ getExpPos exp) ++ " expression is of type \n" ++ (showT type_)
                                                                                  ++ " but should be of type \n" ++ (showT t);
                                                                           else
                                                                              checkFunctionArgs args exps;
                                                          }
                                                     }

getExpPos :: Expr Pos -> Pos
getExpPos e = case e of
                EVar pos _ -> pos
                ELitInt pos _ -> pos
                ELitTrue pos -> pos
                ELitFalse pos -> pos
                EApp pos _ _ -> pos
                EArrAcc pos _ _ -> pos
                EString pos _ -> pos
                Neg pos _ -> pos
                Not pos _ -> pos
                EMul pos _ _ _ -> pos
                EAdd pos _ _ _ -> pos
                ERel pos _ _ _ -> pos
                EAnd pos _ _ -> pos
                EOr pos _ _ -> pos
                AnF pos _ _ _ -> pos

sliceArrayType :: Type a -> [ArrInd b] -> TypeMonad (Type ())
sliceArrayType (Arr _ t sizes) indices = if length(indices) == length(sizes) then
                                               return $ fmap (\c -> ()) t;
                                         else
                                              return (Arr () (fmap (\c -> ()) t) ( map (fmap (\c -> ()))  (drop (length(indices)) sizes)));

getArraySliceType :: Pos -> Ident -> [ArrInd a] -> TypeMonad (Type ())
getArraySliceType p id [] = getVariableType p id;
getArraySliceType pos id indices = do {
                                 (i, _, env) <- ask;
                                 case Map.lookup id env of
                                     Nothing -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " not in scope!";
                                     Just (_, t , _) -> do {
                                        if not (ofArrayType t) then
                                              throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " is not of array type!";
                                        else
                                              sliceArrayType t indices;
                                     }
}


getVariableType :: Pos -> Ident  -> TypeMonad (Type ())
getVariableType pos id = do {
               (i, _, env) <- ask;
               case Map.lookup id env of
                    Just (_, t, True) -> return $ fmap (\c -> ()) t;
                    Just (_, t , False) -> do {
                                                liftIO $ hPutStrLn  stderr ("Warning at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " may not have been initialized!");
                                                return $ fmap (\c -> ()) t;
                    }
                    Nothing -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " not in scope!";
           }

getExpressionType :: Expr Pos -> TypeMonad (Type ())
getExpressionType exp = case exp of
                            ELitInt _ _ -> return $ Int ()
                            ELitTrue _ -> return $ Bool ()
                            ELitFalse _ -> return $ Bool ()
                            EString _ _ -> return $ Str ()
                            Neg pos exp -> do {
                                                t <- getExpressionType exp;
                                                case t of
                                                    Int _ -> return $ Int ();
                                                    _ -> throwError ("Error at position " ++ (show $ fromMaybe (0,0) pos) ++ " expression\n" ++ (showE exp) ++ "\nat position " ++ (show $ fromMaybe (0,0) $ getExpPos exp) ++ " is not of integer type!");
                                            }
                            Not pos exp -> do {
                                            t <- getExpressionType exp;
                                            case t of
                                                Bool _ -> return $ Bool ()
                                                _ -> throwError ("Expression\n" ++ (showE exp) ++ "\nat position " ++ (show $ fromMaybe (0,0) pos) ++ " is not of boolean type!");
                                           }
                            EAnd pos exp1 exp2 -> do {
                                                    t1 <- getExpressionType exp1;
                                                    t2 <- getExpressionType exp2;
                                                    case t1 of
                                                        Bool _ ->
                                                                case t2 of
                                                                    Bool _ -> return $ Bool ()
                                                                    _ -> throwError ("Expression\n" ++ (showE exp2) ++ "\nat position " ++ (show $ fromMaybe (0,0) $ getExpPos exp2) ++ " is not of boolean type!");
                                                        _ -> throwError ("Expression\n" ++ (showE exp1) ++ "\nat position " ++ (show $ fromMaybe (0,0)$ getExpPos exp1) ++ " is not of boolean type!");
                                                  }
                            EOr pos exp1 exp2 -> getExpressionType $ EAnd pos exp1 exp2
                            EVar pos id -> do {
                                               (i, _, env) <- ask;
                                               case Map.lookup id env of
                                                    Just (_, t, True) -> return $ fmap (\c -> ()) t;
                                                    Just (_, t , False) -> do {
                                                                                liftIO $ hPutStrLn  stderr ("Warning at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " may not have been initialized!");
                                                                                return $ fmap (\c -> ()) t;
                                                    }
                                                    Nothing -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " not in scope!";
                                           }
                            EAdd _ exp1 op exp2 -> do {
                                                    t1 <- getExpressionType exp1;
                                                    t2 <- getExpressionType exp2;
                                                    case t1 of
                                                        Int _ ->
                                                                case t2 of
                                                                    Int _ -> return $ Int ()
                                                                    _ -> throwError ("Expression\n" ++ (showE exp2) ++ "\nat position " ++ (show $ fromMaybe (0,0) $ getExpPos exp2) ++ " is not of integer type!");
                                                        _ -> throwError ("Expression\n" ++ (showE exp1) ++ "\nat position " ++ (show $ fromMaybe (0,0) $ getExpPos exp1) ++ " is not of integer type!");
                                                  }
                            EMul p exp1 op exp2 -> getExpressionType $ EAdd p exp1 (Plus p) exp2
                            EArrAcc pos id (exprs) -> do {
                                                        (i, _, env) <- ask;
                                                        catchError  (checkIfInt $ map (\(ArrInd _ exp ) -> exp) exprs) (\e -> throwError $ "Array index must be an integer\n" ++ e);
                                                        case Map.lookup id env of
                                                            Nothing -> throwError ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " not in scope!");
                                                            Just (_, Arr pos2 type_ arrsizes, _) ->
                                                                    if length(arrsizes) /= length(exprs) then
                                                                        throwError ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " is an array of size " ++ (show $ length arrsizes) ++ " but " ++ (show $ length exprs) ++ " indices were given!");
                                                                    else return $ fmap (\c -> ()) type_;
                                                            _ -> throwError ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " is not an array!");
                                                    }
                            ERel pos exp1 op exp2 -> do {
                                                        t1 <- getExpressionType exp1;
                                                        t2 <- getExpressionType exp2;
                                                        if differentTypes t1 t2 then
                                                            throwError ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " expressions are not of the same type!");
                                                        else
                                                            case t1 of
                                                                Int _ -> return $ Bool ();
                                                                Bool _ -> case op of
                                                                                    EQU _-> return $ Bool ();
                                                                                    NE _-> return $ Bool ();
                                                                                    _ -> throwError ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " don't know how to apply operator to the type!");
                                                                Str _ -> case op of
                                                                                    EQU _-> return $ Bool ();
                                                                                    NE _-> return $ Bool ();
                                                                                    _ -> throwError ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " don't know how to apply operator to the type!");
                                                                _ -> throwError ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " don't know how to apply operator to the type!");
                                                     }
                            EApp pos id exps -> do {
                                                    (i, _, env) <- ask;
                                                    case Map.lookup id env of
                                                            Nothing -> throwError ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " function " ++ (show id) ++ " not in scope!");
                                                            Just (_, _, False) -> throwError ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " function " ++ (show id) ++ " not initialized!");
                                                            Just (_, Fun _ (t) args, True) -> do {
                                                                                        if length(args) /= length(exps) then
                                                                                              throwError  ("Error at " ++ (show $ fromMaybe (0,0) pos) ++ " wrong number of parameters to function " ++ (show id));
                                                                                        else
                                                                                            checkFunctionArgs args exps;
                                                                                            return $ fmap (\c -> ()) t;
                                                                                    }
                                                }
                            AnF pos type_ args block -> do {
                                               checkStmts [FnDef pos type_ (Ident "dummy") args block];
                                               return (Fun () (fmap (\c -> ()) type_)  (map (fmap (\c -> ())) (mapArgsToArgT args)));
                            }

addArgsToEnv ::  [Arg Pos] -> TypeEnv -> TypeEnv
addArgsToEnv [] e = e;
addArgsToEnv (Arg _ t id :args) e = addArgsToEnv args $ (\(i, r, env) -> (i, r, Map.insert id (i+1, t, True) env)) e
addArgsToEnv (RefArg _ t id :args) e = addArgsToEnv args $ (\(i, r, env) -> (i, r, Map.insert id (i+1, t, True) env)) e

variableAlreadyDefinedError ::  Pos -> Ident -> Type Pos -> TypeMonad (Type ())
variableAlreadyDefinedError pos id t = throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " has already been defined at " ++ (show $ fromMaybe (0,0) $ getTypePos t);

checkFunctionDefinition :: [Stmt Pos] -> TypeMonad (Type ())
checkFunctionDefinition (FnDef pos t id args (Block _ stmts ):xs)  = do {
                                 (i, _, env) <- ask;
                                 retVal <- local (addArgsToEnv args) $ local(\(ii, tt, envv) -> (ii, tt, Map.insert id (i, Fun pos t $ mapArgsToArgT args, True) envv)) $ local(\(i, _, env) -> (i+1, fmap (\c -> ()) t, env)) $ checkStmts $ filter (\w -> case w of {Empty _ -> False; _ -> True}) stmts;
                                 if differentTypes retVal t then
                                    throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " function " ++ (show id) ++ " is expected to return\n" ++ (showT t) ++ "\nbut returns\n" ++ (showT retVal);
                                 else
                                    local(\(ii, tt, envv) -> (ii, tt, Map.insert id (i, Fun pos t $ mapArgsToArgT args, True) envv)) $ checkStmts xs;
                               }
checkFunctionDefinition _ = undefined

checkStmts :: [Stmt Pos] -> TypeMonad (Type ())
checkStmts [] = return $ Void ();
checkStmts (VRet pos: stmts)  = do {
                                    (_, t, _) <- ask;
                                    if differentTypes t (Void ()) then
                                           throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " return type inferred to void but expected\n" ++ (showT t)
                                    else return $ Void ();
                                   }
checkStmts (Ret pos exp: stmts)  = do {
                                      t <- catchError (getExpressionType exp) (\e -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " couldn't check return value\n" ++ "    " ++ e);
                                      if not $ differentTypes t (Void ()) then
                                             throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " can't return void value";
                                      else do {
                                          (_, tt, _) <- ask;
                                          if differentTypes tt t then
                                               throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " return type inferred to\n" ++ (showT t) ++ "\nbut expected\n" ++ (showT tt)
                                          else return $ fmap (\c -> ()) t;
                                      }
                                   }
checkStmts (While pos exp s: stmts) =
                                   do {
                                      t <- catchError (getExpressionType exp) (\e -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " couldn't check while condition\n" ++ "    " ++ e);
                                      if differentTypes t (Bool ())  then
                                            throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " expression\n" ++ (showE exp) ++ "\nis not of boolean type!";
                                       else do {
                                            local (\(ii, tt, envv) -> (ii, tt,  Map.insert (Ident "while") (0, Void Nothing, True) envv)) $ local (\(i, tt, env) -> (i+1, tt, env)) $ checkStmts [s];
                                            checkStmts stmts;
                                       }
                                   }
checkStmts [CondElse pos exp s1 s2] =
                                   do {
                                      t <- catchError (getExpressionType exp) (\e -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " couldn't check if condition\n" ++ "    " ++ e);
                                      if differentTypes t (Bool ()) then
                                            throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " expression\n" ++ (showE exp) ++ "\nis not of boolean type!";
                                       else do {
                                        local (\(i, tt, env) -> (i+1, tt, env)) $ checkStmts [s1];
                                        local (\(i, tt, env) -> (i+1, tt, env)) $ checkStmts [s2];
                                        }
                                   }
checkStmts (CondElse pos exp s1 s2: stmts) =
                                   do {
                                      t <- catchError (getExpressionType exp) (\e -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " couldn't infer type in if condition\n" ++ "    " ++ e);
                                      if differentTypes t (Bool ()) then
                                            throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " expression\n" ++ (showE exp) ++ "\nis not of boolean type!";
                                       else do {
                                        local (\(i, tt, env) -> (i+1, tt, env)) $ checkStmts [s1];
                                        local (\(i, tt, env) -> (i+1, tt, env)) $ checkStmts [s2];
                                        checkStmts stmts;
                                        }
                                   }
checkStmts (Cond pos exp s1 : stmts) = checkStmts (CondElse pos exp s1 (Empty Nothing): stmts)
checkStmts (Break pos : stmts)  = do {
                                     (i, _, env)  <- ask;
                                     case (Map.lookup (Ident "while") env) of
                                        Nothing -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " this statement must be contained in a while block!";
                                        _ -> checkStmts stmts;
                                 }
checkStmts (Continue pos : stmts) = checkStmts (Break pos: stmts);
checkStmts (Empty pos:stmts) = checkStmts stmts;
checkStmts [BStmt _ (Block _ s)] = do {
                                        local (\(i, tt, env) -> (i+1, tt, env)) $ checkStmts $ filter (\w -> case w of {Empty _ -> False; _ -> True}) s;
                                    }
checkStmts (BStmt _ (Block _ s):xs) = do {
                                        local (\(i, tt, env) -> (i+1, tt, env)) $ checkStmts $ filter (\w -> case w of {Empty _ -> False; _ -> True}) s;
                                        checkStmts xs;
                                    }

checkStmts (Decl _ (Arr pos t sizes) (NoInit _ id) : xs) = do {
                                                       (i, _, env) <- ask;
                                                       if ofArrayType t then
                                                            throwError $ "Error at "++ (show $ fromMaybe (0,0) pos) ++ " arrays of arrays are not allowed!";
                                                       else if (null (filter (<=0) $ map (\(ArrSize _ j) -> j) sizes)) then
                                                             if sameTypes (Void ()) t then
                                                                throwError $ "Error at "++ (show $ fromMaybe (0,0) pos) ++ " arrays of void type are not allowed!";
                                                             else case (Map.lookup id env) of
                                                                  Just (j, t2, _) ->
                                                                        if j == i then
                                                                            variableAlreadyDefinedError pos id t2;
                                                                        else
                                                                            local( \(ii, tt, envv) -> (ii, tt, Map.insert id (i, (Arr pos t sizes), True) envv)) $ checkStmts xs;
                                                                  _ -> local( \(ii, tt, envv) -> (ii, tt, Map.insert id (i, (Arr pos t sizes), True) envv)) $ checkStmts xs;
                                                        else
                                                            throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable indexes should be positive!";
                                                   }
checkStmts (Decl pos t (NoInit pos2 id) : xs) = do {
                                            (i, _, env) <- ask;
                                            case (Map.lookup id env) of
                                                Just (j, t2, _) ->
                                                    if j == i then
                                                        variableAlreadyDefinedError pos id t2;
                                                    else
                                                        local(\(ii, tt, envv) -> (ii, tt , Map.insert id (i, t, False) envv)) $ checkStmts xs;
                                                _ -> local(\(ii, tt, envv) -> (ii, tt , Map.insert id (i, t, False) envv)) $ checkStmts xs;
                                          }
checkStmts (Decl pos (Arr _ _ _) (Init pos2 id exp) : xs) = throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " array variables cannot be initialized at definition!"
checkStmts (Decl pos t (Init pos2 id exp) : xs) = do {
                                            t2 <- getExpressionType exp;
                                            (i, _, env) <- ask;
                                            if differentTypes t t2 then
                                                throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " is of type\n" ++ (showT t) ++ "\nwhile assignment type is\n" ++ (showT t2);
                                            else
                                                 case (Map.lookup id env) of
                                                    Just (j, t3, _) ->
                                                           if j == i then
                                                               variableAlreadyDefinedError pos id t3;
                                                           else
                                                               local(\(ii, tt, envv) -> (ii, tt , Map.insert id (i, t, True) envv)) $ checkStmts xs;
                                                    _ -> local(\(ii, tt, envv) -> (ii, tt , Map.insert id (i, t, True) envv)) $ checkStmts xs;
                                          }
checkStmts (Ass pos id (EArrAcc pos2 id2 indices) : xs) = do {
                                    t <- getArraySliceType pos2 id2 indices;
                                    (i, _, env) <- ask;
                                    case (Map.lookup id env) of
                                        Nothing -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " not in scope!";
                                        Just (j, t2, _) -> if differentTypes t t2 then
                                                                throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable is of type\n" ++ (showT t2) ++ "\nbut assignment expression is of type\n" ++ (showT t);
                                                            else local(\(ii, tt, envv) -> (ii, tt , Map.insert id (j, t2, True) envv)) $ checkStmts xs;
                                }
checkStmts (Ass pos id exp : xs) = do {
                                    t <- getExpressionType exp;
                                    (i, _, env) <- ask;
                                    case (Map.lookup id env) of
                                        Nothing -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " not in scope!";
                                        Just (j, t2, _) -> if differentTypes t t2 then
                                                                throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable is of type\n" ++ (showT t2) ++ "\nbut assignment expression is of type\n" ++ (showT t);
                                                            else local(\(ii, tt, envv) -> (ii, tt , Map.insert id (j, t2, True) envv)) $ checkStmts xs;
                                }

checkStmts (SExp pos (exp) :xs) = do {
                                        catchError (getExpressionType exp) (\e -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " couldn't check expression\n" ++ "    " ++ e);
                                        checkStmts xs;
                                   }
checkStmts (Incr pos id :xs) = do {
                                       (i, _, env) <- ask;
                                       case Map.lookup id env of
                                            Nothing -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++  (show id)  ++ " not in scope!";
                                            Just (_, Arr _ _ _, _) -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " can't apply the operator to arrays!";
                                            Just (j, t, False) -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++  (show id)  ++ " not initialized!";
                                            Just (j, t, True) -> if differentTypes t (Int ()) then
                                                                throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++ (show id) ++ " is not of integer type!";
                                                            else
                                                                checkStmts xs;
                                 }
checkStmts (Decr pos id :xs) = checkStmts (Incr pos id :xs)
checkStmts (FnDef pos (Arr _ _ _ ) id args (Block _ stmts ) : xs) = throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " functions can't return arrays!"
checkStmts (FnDef pos t id args (Block pp stmts ) : xs) = do {
                                                if (not $ allDifferent (map getArgId args)) then
                                                     throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " function arguments have duplicated names";
                                                else do {
                                                    if (hasVoidArgs args) then
                                                        throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " function arguments can't have void type";
                                                     else
                                                        do {
                                                            (i, _, env) <- ask;
                                                            if not $ checkIfArrByRef args then
                                                                throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " arrays can only be passed by reference";
                                                            else
                                                                case (Map.lookup id env) of
                                                                    Just (j, t2, _) ->
                                                                        if i == j then
                                                                            variableAlreadyDefinedError pos id t2
                                                                        else
                                                                            checkFunctionDefinition (FnDef pos t id args (Block pp stmts ):xs);
                                                                    _ -> checkFunctionDefinition (FnDef pos t id args (Block pp stmts ):xs);
                                                   }}
                                                }
checkStmts (ArrInit pos id indices exp : xs) = do {
                                                    (i, _, env) <- ask;
                                                    te <- getExpressionType exp;
                                                    case (Map.lookup id env) of
                                                        Just (_, Arr _ t sizes,  _) ->
                                                                    if length(sizes) < length(indices) then
                                                                        throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " array " ++  (show id)  ++ " has dimension " ++ (show $ length(sizes)) ++ " but " ++ (show $ length(indices)) ++ " were given!";
                                                                    else do {
                                                                        slice_type <- getArraySliceType pos id indices;
                                                                        if differentTypes slice_type te then
                                                                            throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " expected type\n" ++ (showT slice_type) ++ "\nbut got " ++ (showT te)
                                                                        else
                                                                            checkStmts xs;
                                                                        }
                                                        Just _ -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++  (show id)  ++ " not of array type!";
                                                         _ -> throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " variable " ++  (show id)  ++ " not in scope!";
                                                }
checkStmts (Print pos exp : xs) = do {
                                       t <- getExpressionType  exp;
                                       if or [sameTypes t (Int ()), sameTypes t (Bool ()), sameTypes t (Str ())] then
                                            checkStmts xs;
                                       else
                                            throwError $ "Error at " ++ (show $ fromMaybe (0,0) pos) ++ " cannot print expression of type\n" ++ (showT t);
                                    }





validate :: Program Pos -> IO (Either String (Type ()))
validate (Program _ stmts) =    (runReaderT $ runExceptT  $ (checkStmts $ filter (\w -> case w of {Empty _ -> False; _ -> True}) stmts )) (0, Void (), Map.empty)
