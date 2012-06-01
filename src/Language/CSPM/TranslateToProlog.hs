-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.TranslateToProlog
-- Copyright   :  (c) Fontaine 2010 - 2011
-- License     :  BSD3
--
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Translate a CSPM-specification to Prolog.
-- This is the interface used by Prolog
-----------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}
module Language.CSPM.TranslateToProlog
(
   toPrologVersion
  ,translateToProlog
)
where

import Language.CSPM.Frontend as Frontend
import qualified Language.CSPM.SrcLoc as SrcLoc
import qualified Language.CSPM.Token as Token (lexEMsg,lexEPos,alexLine,alexCol,alexPos)
import Language.CSPM.CompileAstToProlog (cspToProlog,mkSymbolTable)
import Language.CSPM.AstToProlog (toProlog)
import Language.Prolog.PrettyPrint.Direct
import Paths_CSPM_ToProlog (version)
import Data.Version (Version,showVersion)

import Control.Exception
import System.Exit
import System.IO
import System.CPUTime
import Text.PrettyPrint

-- | The version of the CSPM-ToProlog library
toPrologVersion :: Version
toPrologVersion = version

-- | 'translateToProlog' reads a CSPM specification from inFile
-- and writes the Prolog representation to outFile.
-- It handles all lexer and parser errors and catches all exceptions.
translateToProlog ::
     FilePath -- ^ filename input
  -> FilePath -- ^ filename output
  -> IO ()
translateToProlog inFile outFile = do
  res <- handle catchAllExceptions
          $ handleLexError lexErrorHandler
             $ handleParseError parseErrorHandler
               $ handleRenameError renameErrorHandler $ mainWork inFile
  (r :: Either SomeException ()) <- try $ writeFile outFile res
  case r of
    Right () -> exitSuccess
    Left err -> do
      hPutStrLn stderr "output-file not written"
      hPutStrLn stderr $ show err
      exitFailure

{-
main :: IO ()
main = do
  args <- getArgs
  case args of
    [inFile,outFile] -> do
      translateToProlog inFile outFile
      exitSuccess
    _ -> do
      putStrLn "Start with two arguments (input filename and output filename)"
      exitFailure
-}

mainWork :: FilePath -> IO String
mainWork fileName = do
  src <- readFile fileName

  printDebug $ "Reading File " ++ fileName
  startTime <- (return $ length src) >> getCPUTime
  tokenList <- lexInclude src >>= eitherToExc
  time_have_tokens <- getCPUTime

  ast <- eitherToExc $ parse fileName tokenList
  time_have_ast <- getCPUTime

  printDebug $ "Parsing OK"
  printDebug $ "lextime : " ++ showTime (time_have_tokens - startTime)
  printDebug $ "parsetime : " ++ showTime(time_have_ast - time_have_tokens)
  
  time_start_renaming <- getCPUTime
  (astNew, renaming) <- eitherToExc $ renameModule ast
  let
      plCode = cspToProlog astNew
      symbolTable = mkSymbolTable $ identDefinition renaming
      moduleFact  = toProlog astNew
  output <- evaluate $ show $ vcat [ 
      mkResult "ok" "" 0 0 0
     ,moduleFact
     ,plCode
     ,symbolTable
     ]

  time_have_renaming <- getCPUTime
  printDebug $ "renamingtime : " ++ showTime (time_have_renaming - time_start_renaming)
  printDebug $ "total : " ++ showTime(time_have_ast - startTime)
  return output

showTime :: Integer -> String
showTime a = show (div a 1000000000) ++ "ms"

defaultHeader :: Doc
defaultHeader 
  =    text ":- dynamic parserVersionNum/1, parserVersionStr/1, parseResult/5."
    $$ text ":- dynamic module/4."
    $$ simpleFact "parserVersionStr" [aTerm $ showVersion toPrologVersion]

simpleFact :: String -> [Term] -> Doc
simpleFact a l= plPrg [declGroup [clause $ nTerm a l]]

mkResult :: String -> String -> Int -> Int -> Int -> Doc
mkResult var msg line col pos
  = defaultHeader   
    $$ simpleFact "parseResult" [aTerm var, aTerm msg, aTerm line, aTerm col, aTerm pos]

printDebug :: String -> IO ()
printDebug _ = return ()
--printDebug = putStrLn

parseErrorHandler :: ParseError -> IO String
parseErrorHandler err = do
  printDebug "ParseError : "
  printDebug $ show err
  let loc = Frontend.parseErrorPos err
  evaluate $ show
    $ mkResult "parseError"
        (Frontend.parseErrorMsg err)
        (Token.alexLine loc)
        (Token.alexCol loc)
        (Token.alexPos loc)

lexErrorHandler :: LexError -> IO String
lexErrorHandler err = do
  printDebug "LexError : "
  printDebug $ show err
  let loc = Token.lexEPos err
  evaluate $ show
    $ mkResult "lexError"
        (Token.lexEMsg err)
        (Token.alexLine loc)
        (Token.alexCol loc)
        (Token.alexPos loc)

renameErrorHandler :: RenameError -> IO String
renameErrorHandler err = do 
  printDebug "RenamingError : "
  printDebug $ show err
  let loc = Frontend.renameErrorLoc err
  evaluate $ show
    $ mkResult "renameError"
        (Frontend.renameErrorMsg err)
        (SrcLoc.getStartLine loc)
        (SrcLoc.getStartCol loc)
        (SrcLoc.getStartOffset loc)

catchAllExceptions :: SomeException -> IO String
catchAllExceptions err = do
  printDebug "ParserException : "
  printDebug $ show err
  evaluate $ show $ mkResult "exception" (show err) 0 0 0
