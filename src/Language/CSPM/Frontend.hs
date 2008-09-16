-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Frontend
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Frontend contains some reexports from other modules

module Language.CSPM.Frontend
(
   module Language.CSPM.AST
  ,testFrontend
  ,parseFile
  ,Lexer.lexInclude
  ,Lexer.lexPlain
  ,Lexer.filterIgnoredToken
  ,LexError(..)
  ,Parser.parse
  ,Parser.ParseError(..)
  ,Rename.getRenaming
  ,Rename.applyRenaming
  ,Rename.RenameError(..)
  ,errToExc
  ,handleLexError
  ,handleParseError
  ,handleRenameError
  ,version
)
where
import Language.CSPM.Token
import Language.CSPM.AST
import qualified Language.CSPM.Parser as Parser
import qualified Language.CSPM.Rename as Rename

import qualified Language.CSPM.LexHelper as Lexer
  (lexInclude,lexPlain,filterIgnoredToken)
import Language.CSPM.Version

import Control.Exception
import System.CPUTime
import Data.Typeable

errToExc :: Typeable a => Either a b -> IO b
errToExc (Right r) = return r
errToExc (Left e) = throwDyn e

handleLexError :: (LexError -> IO a) -> IO a -> IO a
handleLexError handler proc = catchDyn proc handler

handleParseError :: (Parser.ParseError -> IO a) -> IO a -> IO a
handleParseError handler proc = catchDyn proc handler

handleRenameError :: (Rename.RenameError -> IO a) -> IO a -> IO a
handleRenameError handler proc = catchDyn proc handler

parseFile :: FilePath -> IO LModule
parseFile fileName = do
  src <- readFile fileName
  tokenList <- Lexer.lexInclude src >>= errToExc
  errToExc $ Parser.parse fileName tokenList

testFrontend :: FilePath -> IO (LModule,LModule)
testFrontend fileName = do
  src <- readFile fileName

  putStrLn $ "Reading File " ++ fileName
  startTime <- (return $ length src) >> getCPUTime
  tokenList <- Lexer.lexInclude src >>= errToExc
  time_have_tokens <- getCPUTime

  ast <- errToExc $ Parser.parse fileName tokenList
  time_have_ast <- getCPUTime

  renaming <- errToExc $ Rename.getRenaming ast
  let astNew = Rename.applyRenaming renaming ast
  time_have_renaming <- getCPUTime

  putStrLn $ "Parsing OK"
  putStrLn $ "lextime : " ++ showTime (time_have_tokens - startTime)
  putStrLn $ "parsetime : " ++ showTime(time_have_ast - time_have_tokens)
  putStrLn $ "renamingtime : " ++ showTime (time_have_renaming - time_have_ast)
  putStrLn $ "total : " ++ showTime(time_have_ast - startTime)
  return (ast,astNew)

showTime :: Integer -> String
showTime a = show (div a 1000000000) ++ "ms"
