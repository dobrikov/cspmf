#!/usr/bin/env runhaskell
{-
first argument is the name of cspm-file
read file, parse it and write the result to a File
-}

import Language.CSPM.Parser
import Language.CSPM.Rename

import Control.Exception
import System.Environment
import System.Cmd
import System.Exit
import System.IO
import Control.Monad
import System.CPUTime
import Data.Typeable

errToExc :: Typeable a => Either a b -> IO b
errToExc (Right r) = return r
errToExc (Left e) = throwDyn e

main
  = flip catchDyn handleLexError
      $ flip catchDyn handleParseError
        $ flip catchDyn handleRenameError $
  do
  args <- getArgs
  when (null args) $ do
    putStrLn "Start with a filename argument"
    exitFailure

  let fileName = head args
  src <- readFile fileName

  putStrLn $ "Reading File " ++ fileName
  startTime <- (return $ length src) >> getCPUTime
  tokenList <- lexInclude src >>= errToExc
  time_have_tokens <- getCPUTime

  ast <- errToExc $ parseCSP fileName tokenList
  time_have_ast <- getCPUTime

  putStrLn $ "Parsing OK"
  putStrLn $ "lextime : " ++ show (div (time_have_tokens - startTime) 1000000000)
  putStrLn $ "parsetime : " ++ show (div (time_have_ast - time_have_tokens) 1000000000)
  putStrLn $ "total : " ++ show (div (time_have_ast - startTime)  1000000000)

  writeFile (fileName ++ ".ast") $ show ast

  renaming <- errToExc $ renameModule ast
  let astNew = applyRenaming renaming ast
  writeFile (fileName ++ ".ast2") $ show astNew
  exitWith ExitSuccess

handleParseError :: ParseError -> IO ()
handleParseError err = do
  putStrLn "ParseError : "
  putStrLn $ show err
  exitFailure

handleLexError :: LexError -> IO ()
handleLexError err = do
  putStrLn "LexError : "
  putStrLn $ show err
  exitFailure

handleRenameError :: RenameError -> IO ()
handleRenameError err = do 
  putStrLn "RenamingError : "
  putStrLn $ show err
  exitFailure

{-
to build all ASTs for the testcases do

cd cspm
for i in $(ls *.csp *.fdr2) ; do ../runCspParser.hs $i ; done
-}