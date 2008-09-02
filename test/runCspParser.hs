#!/usr/bin/env runhaskell
{-
first argument is the name of cspm-file
read file, parse it and print AST on stdout
-}

import Language.CSPM.Parser

import System.Environment
import System.Cmd
import System.Exit
import Control.Monad

main = do
  putStrLn "runCspParser.hs"
  args <- getArgs
  when (null args) $ do
    putStrLn "Start with a filename argument"
    exitFailure
  let fileName = head args
  src <- readFile fileName
  t <- runScannerInclude src
  case t of
    Left e -> do
      putStrLn $ "LexError : " ++ show e
      exitFailure
    Right tl -> case runCspParser fileName tl of
      Left e -> do
        putStrLn $ "ParseError : " ++ show e
        exitFailure
      Right ast -> do
        putStrLn $ show ast
        exitWith ExitSuccess
