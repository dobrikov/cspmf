----------------------------------------------------------------------------
-- |
-- Module      :  Main.ExecCommand
-- Copyright   :  (c) Fontaine 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Comand line interface for the CSPM tools.
----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

module Main.ExecCommand
where

import Main.Args (Args(..))
import Paths_CSPM_cspm_frontend (version)

import Language.CSPM.Frontend
  (parseFile, frontendVersion
  ,eitherToExc, renameModule, castModule, lexInclude)
import Language.CSPM.LexHelper (unicodeTokenString,asciiTokenString)
--import Language.CSPM.PrettyPrinter (pPrint)

import Language.CSPM.TranslateToProlog
  (translateToProlog,toPrologVersion
  ,translateExpToPrologTerm,translateDeclToPrologTerm,translateToPrologNormalised)
import Language.CSPM.AstToXML (moduleToXML, showTopElement)

import Text.PrettyPrint.HughesPJClass
import System.IO
import Data.Version (showVersion)
import Control.Monad
import Data.Maybe
import Data.List as List

{-
instance EqOrd INT
instance CSP1 INT
instance CSP2 INT
instance FShow INT
-}

-- | execute the command according to command line arguments
execCommand :: Args -> IO ()
execCommand Info {..} = do
  putStr $ concat
    [
     "Versions :",nl
    ,"  cspmf command line utility : ", showVersion version, nl
    ,"  CSPM-Frontend              : ", showVersion frontendVersion, nl
    ,"  CSPM-ToProlog              : ", showVersion toPrologVersion, nl
    ,nl
    ,"Usage examples:",nl
    ,"  cspmf --help",nl
    ,"  cspmf info",nl
    ,"  cspmf translate",nl
    ,nl
    ,"Copyright (c) Marc Fontaine, Ivaylo Dobrikov 2007-2015",nl
    ,"Email : Marc.Fontaine@gmx.de, ivaylo.dobrikov@googlemail.com",nl
    ]
  where nl = "\n"

execCommand Translate {..} = do
  when (null $ catMaybes
     [prologOut, xmlOut, prettyOut, addUnicode, removeUnicode, expressionToPrologTerm, declarationToPrologTerm]) $ do
    putStrLn "No output option is set"
    putStrLn "Set '--xmlOut', '--prettyOut' or an other output option"
  when (isJust xmlOut || isJust prettyOut) $ do
    -- AST transformations
      ast <- do
          ast1 <- parseFile src
          if rename
            then fmap fst $ eitherToExc $ renameModule ast1
            else return $ castModule ast1
      whenJust xmlOut $ \outFile -> do
          writeFile outFile $ showTopElement $ moduleToXML ast
      whenJust prettyOut $ \outFile -> do
          writeFile outFile $ prettyShow ast
  when (isJust addUnicode || isJust removeUnicode) $ do
    -- Token stream transformations
      tokens <- readFile src >>= lexInclude src >>= eitherToExc
      whenJust addUnicode $ \outFile -> withFile outFile WriteMode
         $ \handle -> do
             hSetEncoding handle utf8
             hPutStr handle $ List.concatMap unicodeTokenString tokens
             hClose handle
      whenJust removeUnicode $ \outFile -> do
          writeFile outFile $ List.concatMap asciiTokenString tokens
  whenJust prologOut $ \outFile -> do
      translateToProlog src outFile -- translateToProlog does not return !
      error "unreachable"
  whenJust prologOutNormalised $ \outFile -> do
      translateToPrologNormalised src outFile -- translateToProlog does not return !
      error "unreachable"
    -- Incremental parser features
  whenJust expressionToPrologTerm $ \str -> do
      let srcFile = if src == "no-file" then Nothing else Just src
      translateExpToPrologTerm srcFile str
  whenJust declarationToPrologTerm $ \str -> do
      let srcFile = if src == "no-file" then Nothing else Just src
      translateDeclToPrologTerm srcFile str
  where
    whenJust a action = case a of
      Just v -> action v
      Nothing -> return ()

