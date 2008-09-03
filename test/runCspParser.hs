#!/usr/bin/env runhaskell
{-
first argument is the name of cspm-file
read file, parse it and print AST on stdout
-}

import Language.CSPM.Parser

import System.Environment
import System.Cmd
import System.Exit
import System.IO
import Control.Monad

main = do
  args <- getArgs
  when (null args) $ do
    putStrLn "Start with a filename argument"
    exitFailure
  let fileName = head args
  src <- readFile fileName
  t <- lexInclude src
  case t of
    Left e -> do
      putStrLn $ "LexError : " ++ show e
      exitFailure
    Right tl -> case parseCSP fileName tl of
      Left e -> do
        putStrLn $ "ParseError : " ++ show e
        exitFailure
      Right ast -> do
        putStrLn $ show ast
        hFlush stderr
        hFlush stdout
        exitWith ExitSuccess

 

{-
to build all ASTs for the testcases do

cd cspm
for i in $(ls *) ; do echo $i ; ../runCspParser.hs $i >../ast/$i.ast ; done
-}