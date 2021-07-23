module InterpretTypes where
import AbsGramatyka
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map

type Pos = Maybe (Int, Int)

type Loc = Integer

data FuncArg = L Loc | E (ExpValue)

newtype Function = Function ([Bool] , ([FuncArg] -> ProgramMonad ExpValue), Env)

data ExpValue = NoReturnEV | VoidEV | IEV Integer | SEV String | BEV Bool
                       | FEV (Function) | ContinueEV | BreakEV | ArrEV (Integer, Loc)

type Env = Map Ident Loc

type PState = (Loc, Map Loc ExpValue)

type ProgramMonad a = ExceptT String (ReaderT Env (StateT PState  (IO ))) a


modifyState :: Loc -> ExpValue -> PState ->PState
modifyState l exp (l2, s) = (l2, Map.insert l exp s)


alloc :: ProgramMonad Loc
alloc = do {
            (l, s) <- get;
            put (l+1,s);
            return l;
        }

allocForArgs :: [Arg a] -> ProgramMonad [Loc]
allocForArgs [] = return []
allocForArgs (Arg _ _ id :args) = do {
                                    newloc <- alloc;
                                    locs <- allocForArgs args;
                                    return (newloc : locs);
                                }
allocForArgs (RefArg _ _ id :args) = do {
                                    locs <- allocForArgs args;
                                    return (-1: locs);
                                }

getVarLoc :: Ident -> ProgramMonad Loc
getVarLoc id = do {
                    env <- ask;
                    case (Map.lookup id env) of
                        Nothing -> throwError $ "Variable " ++ (show id) ++ " has no location"
                        Just l -> return l;
               }



getLocValue :: Loc -> ProgramMonad ExpValue
getLocValue loc = do {
                    (l, s) <- get;
                    case (Map.lookup loc s) of
                        Nothing -> throwError $ "Runtime error - location is not initialized!";
                        Just e -> return e;
                  }

getVarValue :: Ident -> ProgramMonad ExpValue
getVarValue id = do {
                        loc <- getVarLoc id;
                        getLocValue loc;
                    }

liftMList :: (Monad m) => [m a] -> m [a]
liftMList [] = return [];
liftMList (ma:mas) = do {
                        a <- ma;
                        al <- liftMList mas;
                        return (a:al);
                     }
