#!/usr/bin/env runhaskell
{-
first argument is the name of cspm-file
read file, parse it and write the result to a File
-}

import Language.CSPM.Frontend

import Control.Exception
import System.Environment
import System.Cmd
import System.Exit
import System.IO
import Control.Monad
import System.CPUTime
import Data.Typeable

main
  = handleLexError lexErrorHandler
      $ handleParseError parseErrorHandler
        $ handleRenameError renameErrorHandler $
  do
  args <- getArgs
  when (null args) $ do
    putStrLn "Start with a filename argument"
    exitFailure
  ast <- parseFile $ head args
  renaming <- eitherToExc $ getRenaming ast
  let astNew = applyRenaming renaming ast
  let smallAst = removeSourceLocations $ unUniqueIdent $ removeModuleTokens $ astNew
--  putStrLn $ showAst smallAst
--  putStrLn $ showAst $ compilePattern $ astNew
  putStrLn $ show $ compilePattern $ smallAst
  exitWith ExitSuccess

showTime :: Integer -> String
showTime a = show (div a 1000000000) ++ "ms"

parseErrorHandler :: ParseError -> IO ()
parseErrorHandler err = do
  putStrLn "ParseError : "
  putStrLn $ show err
  exitFailure

lexErrorHandler :: LexError -> IO ()
lexErrorHandler err = do
  putStrLn "LexError : "
  putStrLn $ show err
  exitFailure

renameErrorHandler :: RenameError -> IO ()
renameErrorHandler err = do 
  putStrLn "RenamingError : "
  putStrLn $ show err
  exitFailure
