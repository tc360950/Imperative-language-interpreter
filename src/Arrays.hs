module Arrays where
import InterpretTypes
import ExprUtils
import AbsGramatyka
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

getArrValue :: Pos -> [Integer] -> Loc -> ProgramMonad ExpValue
getArrValue pos is l = do {
                        loc <- getArrValueLoc pos is l;
                        (getLocValue loc) `catchError` (\e -> throwError $ "Runtime error at " ++ (show $ fromMaybe (0,0) pos) ++ " at array access with indices " ++ (show is) ++ "\n"  ++ e);
                    }


getArrValueLoc :: Pos -> [Integer] -> Loc -> ProgramMonad Loc
getArrValueLoc pos [] l = return l;
getArrValueLoc pos [i] l = do {
                        (size, basePtr) <- getArrLocValue l;
                        if (i >= size) then
                            throwError $ "Runtime error at " ++ (show $ fromMaybe (0,0) pos) ++ " array index out of bounds - " ++ (show i) ++ " index access with size " ++ (show size) ++ "!";
                        else
                            return (basePtr+i);
                    }
getArrValueLoc pos (i:is) l = do {
                            (size, basePtr) <- getArrLocValue l;
                            if (i >= size) then
                                throwError $ "Runtime error at " ++ (show $ fromMaybe (0,0) pos) ++ " array index out of bounds - " ++ (show i) ++ " index access with size " ++ (show size) ++ "!";
                            else
                                getArrValueLoc pos is (basePtr+i);
                        }

getArrLocValue :: Loc -> ProgramMonad (Integer, Loc)
getArrLocValue l = do {
                        val <- getLocValue l;
                        case val of
                             ArrEV z -> return z;
                             _ -> throwError "Array crash";
                    }

allocArray :: Loc -> [Integer] -> ProgramMonad ()
allocArray l [] = return ();
allocArray l (i:is) = do {
                    locs <- alloc1DArray l i;
                    liftMListEmpty (map(\loc -> allocArray loc is) locs);
                    }


allocSequence :: Integer -> ProgramMonad Loc;
allocSequence i = do {
            (l, s) <- get;
            put (l+i,s);
            return l;
        }

alloc1DArray :: Loc -> Integer -> ProgramMonad [Loc]
alloc1DArray arrPointer i = do {
                basePointer <- allocSequence i;
                modify (modifyState arrPointer $ ArrEV (i, basePointer));
                return [basePointer.. (basePointer + i -1)];
                }

liftMListEmpty :: (Monad m) => [m ()] -> m ()
liftMListEmpty [] = return ()
liftMListEmpty (ma:ms) = do {
                        ma;
                        liftMListEmpty ms;
                    }

