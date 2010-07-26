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

  let
    lexTime   = (time_have_tokens - startTime)
    parseTime = (time_have_ast - time_have_tokens)
    parseLex  = (time_have_ast - startTime)
    tokPerSec :: Double
    tokPerSec = 1000 * (fromIntegral $ length tokenList)
                / (fromIntegral $ div parseLex 1000000000)
  putStrLn $ "Parsing OK"
  putStrLn $ "size             : " ++ (show $ length src)
  putStrLn $ "loc              : " ++ (show $ length $ lines src) 
  putStrLn $ "lextime          : " ++ showTime lexTime
  putStrLn $ "parsetime        : " ++ showTime parseTime
  putStrLn $ "parser + lexer   : " ++ showTime parseLex
  putStrLn $ "number of tokens : " ++ (show $ length tokenList)
  putStrLn $ "tokens per second: " ++ show tokPerSec

  writeFile (fileName ++ ".ast") $ show ast
  let smallAst = removeSourceLocations $ removeModuleTokens $ ast
  writeFile (fileName ++ ".clean.ast") $ showAst smallAst
  
  time_start_renaming <- getCPUTime
  renaming <- eitherToExc $ getRenaming ast
  let astNew = applyRenaming renaming ast
  case astNew of Labeled {} -> return () -- force astNew ?
  time_have_renaming <- getCPUTime
  putStrLn $ "renamingtime : " ++ showTime (time_have_renaming - time_start_renaming)

  writeFile (fileName ++ ".rename.ast") $ show astNew
  let smallAst = removeSourceLocations $ unUniqueIdent $ removeModuleTokens $ astNew
  writeFile (fileName ++ ".clean.ast") $ showAst smallAst
  let astPatComp = compilePattern astNew
  writeFile (fileName ++ ".patcomp") $ show astPatComp
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