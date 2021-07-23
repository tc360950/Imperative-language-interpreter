module Functions where

import InterpretTypes
import ExprUtils
import AbsGramatyka
import Arrays
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Map as Map

getReferenceMarks :: [Arg pos] -> [Bool] 
getReferenceMarks args = map (\a -> case a of { Arg _ _ _ -> False; RefArg _ _ _ -> True;}) args 

        
getArgId :: Arg a  -> Ident
getArgId (Arg _ t id) = id 
getArgId (RefArg _ t id) = id 


zipLocs :: [Loc] -> [FuncArg] -> [Loc]
zipLocs ls f = zipWith (\l -> \f -> case f of {L l2 -> l2; _ -> l}) ls f

