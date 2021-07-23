module Main where

import System.IO
import System.Environment

import LexGramatyka
import ParGramatyka
import AbsGramatyka

import ErrM

import Interpreter
import TypeValidator


main = do
  args <- getArgs
  fd <- openFile (head args) ReadMode
  program <- hGetContents fd
  case pProgram (myLexer program) of
    Bad errMsg -> hPutStrLn stderr errMsg
    Ok prog -> do
      res <- validate prog
      case res of
        Left error -> hPutStrLn stderr error
        Right _ -> interpret prog
