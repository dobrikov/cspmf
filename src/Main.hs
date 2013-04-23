----------------------------------------------------------------------------
-- |
-- Module      :  Main
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

module Main
where

import Main.ExceptionHandler (handleException)
import Main.Args (argParser)
import Main.ExecCommand (execCommand)

import System.Console.CmdArgs

import System.Exit (exitSuccess)

-- | main-funtion for the command line.
main :: IO ()
main = do
  arguments <- cmdArgsRun argParser
  handleException $ execCommand arguments
  exitSuccess
