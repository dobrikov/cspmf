-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Utils
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Some Utilities

module Language.CSPM.Utils
 (eitherToExc
 ,handleLexError
 ,handleParseError
 ,handleRenameError
 ,parseFile, benchmarkFrontend, parseString)
where

import Language.CSPM.Parser (ParseError(..), parse)
import Language.CSPM.Rename (RenameError(..), renameModule, ModuleFromRenaming)
import Language.CSPM.Token (LexError(..))
import Language.CSPM.AST (ModuleFromParser)
import qualified Language.CSPM.LexHelper as Lexer (lexInclude)

import Control.Exception as Exception
import System.CPUTime

-- | "eitherToExe" returns the Right part of "Either" or throws the Left part as an dynamic exception.
eitherToExc :: Exception a => Either a b -> IO b
eitherToExc (Right r) = return r
eitherToExc (Left e) = throw e

-- | Handle a dymanic exception of type "LexError".
handleLexError :: (LexError -> IO a) -> IO a -> IO a
handleLexError handler proc = Exception.catch proc handler

-- | Handle a dymanic exception of type "ParseError".
handleParseError :: (ParseError -> IO a) -> IO a -> IO a
handleParseError handler proc = Exception.catch proc handler

-- | Handle a dymanic exception of type "RenameError".
handleRenameError :: (RenameError -> IO a) -> IO a -> IO a
handleRenameError handler proc = Exception.catch proc handler

-- | Lex and parse a file and return a "LModule", throw an exception in case of an error
parseFile :: FilePath -> IO ModuleFromParser
parseFile fileName = do
  src <- readFile fileName
  parseNamedString fileName src

-- | Small test function which just parses a String.
parseString :: String -> IO ModuleFromParser
parseString = parseNamedString "no-file-name"

parseNamedString :: FilePath -> String -> IO ModuleFromParser
parseNamedString name str = do
  tokenList <- Lexer.lexInclude str >>= eitherToExc
  eitherToExc $ parse name tokenList

-- | Lex and parse File.
-- | Return the module and print some timing infos
benchmarkFrontend :: FilePath -> IO (ModuleFromParser, ModuleFromRenaming)
benchmarkFrontend fileName = do
  src <- readFile fileName

  putStrLn $ "Reading File " ++ fileName
  startTime <- (return $ length src) >> getCPUTime
  tokenList <- Lexer.lexInclude src >>= eitherToExc
  time_have_tokens <- getCPUTime

  ast <- eitherToExc $ parse fileName tokenList
  time_have_ast <- getCPUTime

  (astNew, _renaming) <- eitherToExc $ renameModule ast
  time_have_renaming <- getCPUTime

  putStrLn $ "Parsing OK"
  putStrLn $ "lextime : " ++ showTime (time_have_tokens - startTime)
  putStrLn $ "parsetime : " ++ showTime(time_have_ast - time_have_tokens)
  putStrLn $ "renamingtime : " ++ showTime (time_have_renaming - time_have_ast)
  putStrLn $ "total : " ++ showTime(time_have_ast - startTime)
  return (ast,astNew)
  where
    showTime :: Integer -> String
    showTime a = show (div a 1000000000) ++ "ms"
