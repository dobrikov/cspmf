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
import System.CPUTime

main = do
  args <- getArgs
  when (null args) $ do
    putStrLn "Start with a filename argument"
    exitFailure
  let fileName = head args
  src <- readFile fileName
  putStrLn $ "Reading File " ++ fileName
  startTime <- (return $ length src) >> getCPUTime
  t <- lexInclude src
  case t of
    Left e -> do
      putStrLn $ "LexError : " ++ show e
      exitFailure
    Right tl -> do
     time_have_tokens <- getCPUTime
     case parseCSP fileName tl of
      Left e -> do
        putStrLn $ "ParseError : " ++ show e
        exitFailure
      Right ast -> do
        time_have_ast <- getCPUTime
        putStrLn $ "Parsing OK"
        putStrLn $ "lextime : " ++ show (div (time_have_tokens - startTime) 1000000000)
        putStrLn $ "parsetime : " ++ show (div (time_have_ast - time_have_tokens) 1000000000)
        putStrLn $ "total : " ++ show (div (time_have_ast - startTime)  1000000000)
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
for i in $(ls *.csp *.fdr2) ; do ../runCspParser.hs $i ; done
-}