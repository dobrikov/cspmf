----------------------------------------------------------------------------
-- |
-- Module      :  Main.Args
-- Copyright   :  (c) Fontaine 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Argument parser for the command line interface
----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main.Args
(
  Args(..)
 ,argParser
)
where

import System.Console.CmdArgs
import Paths_CSPM_cspm_frontend (version)
import Data.Version (showVersion)

-- | Command line argument parser using cmdargs library.
argParser :: Mode (CmdArgs Args)
argParser = cmdArgsMode
  $ modes [
        infoMode, translateMode]
  &= program "cspm"
  &= summary ("cspm command line utility version " ++ showVersion version)

data Args =
   Info {
     verbose :: Bool
    }
  |Translate {
     src    :: FilePath
    ,rename :: Bool
    ,xmlOut :: Maybe FilePath
    ,prettyOut     :: Maybe FilePath
    ,addUnicode    :: Maybe FilePath
    ,removeUnicode :: Maybe FilePath
    ,prologOut     :: Maybe FilePath
    ,expressionToPrologTerm  :: Maybe String
    ,declarationToPrologTerm :: Maybe String
    }
   deriving (Data,Typeable,Show,Eq)

infoMode :: Args
infoMode = Info {
   verbose = def
      &= help "verbose"
      &= name "v" &= name "verbose"
  } &= details ["print some information about the program"] &= auto

translateMode :: Args
translateMode = Translate {
   src = def
     &= typFile
     &= argPos 0
  ,rename = False
     &= help "run renaming  on the AST"
     &= explicit &= name "rename"
  ,xmlOut = def
     &= help "optional: write a file with containing XML"
     &= typFile
     &= explicit &= name "xmlOut"
  ,addUnicode = def
     &= help "optional: replace some CSPM symbols with unicode"
     &= typFile
     &= explicit &= name "addUnicode"
  ,removeUnicode = def
     &= help "optional: replace some unicode symbols with default CSPM encoding"
     &= typFile
     &= explicit &= name "removeUnicode"
  ,prettyOut = def
     &= help "optional: prettyPrint to a file"
     &= typFile
     &= explicit &= name "prettyOut"
  ,prologOut = def
     &= help "translate a CSP-M file to Prolog"
     &= typFile
     &= explicit &= name "prologOut"
  ,expressionToPrologTerm = def
     &= help "translate a single CSP-M expression to Prolog"
     &= typ "STRING"
     &= explicit &= name "expressionToPrologTerm"
  ,declarationToPrologTerm = def
     &= help "translate a single CSP-M declaration to Prolog"
     &= typ "STRING"
     &= explicit &= name "declarationToPrologTerm"
  } &= details ["Parse a specification and write the parse result to a file."]
