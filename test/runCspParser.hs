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

  let fileName = head args
  src <- readFile fileName

  putStrLn $ "Reading File " ++ fileName
  startTime <- (return $ length src) >> getCPUTime
  tokenList <- lexInclude src >>= eitherToExc
  time_have_tokens <- getCPUTime

  ast <- eitherToExc $ parse fileName tokenList
  time_have_ast <- getCPUTime

  renaming <- eitherToExc $ getRenaming ast
  let astNew = applyRenaming renaming ast
  case astNew of Labeled {} -> return () -- force astNew ?
  time_have_renaming <- getCPUTime

  putStrLn $ "Parsing OK"
  putStrLn $ "lextime : " ++ showTime (time_have_tokens - startTime)
  putStrLn $ "parsetime : " ++ showTime(time_have_ast - time_have_tokens)
  putStrLn $ "renamingtime : " ++ showTime (time_have_renaming - time_have_ast)
  putStrLn $ "total : " ++ showTime(time_have_ast - startTime)


  writeFile (fileName ++ ".ast") $ show ast
  writeFile (fileName ++ ".rename.ast") $ show astNew
  writeFile (fileName ++ ".clean.ast") $ show $ removeSourceLocations astNew

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

{-
to build all ASTs for the testcases do

cd cspm
for i in $(ls *.csp *.fdr2) ; do ../runCspParser.hs $i ; done
-}