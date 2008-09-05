#!/usr/bin/env runhaskell
{-
first argument is the name of cspm-file
read file, parse it and write the result to a File
-}

import Language.CSPM.Parser
import Language.CSPM.Rename

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
  putStrLn $ "Reading File " ++ src
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
        putStrLn $ "Parsing OK"
        writeFile (fileName ++ ".ast") $ show ast
--        putStrLn $ show ast
        case renameModule ast of
          Left e -> do
            putStrLn $ "RenamingError : " ++ show e
            exitFailure
          Right r -> do
            putStrLn $ "Renaming OK"
--            putStrLn $ "Renaming: " ++ show r
            let astNew = applyRenaming r ast
            writeFile (fileName ++ ".ast2") $ show astNew
            exitWith ExitSuccess

 

{-
to build all ASTs for the testcases do

cd cspm
for i in $(ls *) ; do echo $i ; ../runCspParser.hs $i ; done
-}