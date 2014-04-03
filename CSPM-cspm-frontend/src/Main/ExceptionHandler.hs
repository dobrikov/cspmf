----------------------------------------------------------------------------
-- |
-- Module      :  Main.ExceptionHandler
-- Copyright   :  (c) Fontaine 2011
-- License     :  BSD3
--
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- ExceptionHandler for the command line interface
----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}

module Main.ExceptionHandler
(
  handleException
)

where

import Language.CSPM.Frontend
  (LexError(..), ParseError(..), RenameError(..))
import Language.CSPM.Token (pprintAlexPosn, Token(..))

import Control.Exception
import System.Exit (exitFailure, ExitCode)
import System.IO

-- | The top-level exception handler.
handleException :: IO () -> IO ()
handleException x
  = x `catches` allHandler
  where
    putStrLnErr = hPutStrLn stderr
    allHandler = [
        Handler propagateExitCode
       ,Handler lexError, Handler parseError, Handler renameError
       ,Handler errCall
       ,Handler async
       ,Handler ioExc
       ,Handler someExc ]
    propagateExitCode :: ExitCode -> IO ()
    propagateExitCode = throwIO
    lexError :: LexError -> IO ()
    lexError LexError {..} = do
      putStrLnErr "lexError"
      putStrLnErr $ pprintAlexPosn lexEPos
      putStrLnErr lexEMsg
      exitFailure
    parseError :: ParseError -> IO ()
    parseError ParseError {..}  = do
      putStrLnErr "parseError"
      putStrLnErr parseErrorMsg
      putStrLnErr $ pprintAlexPosn parseErrorPos
      putStrLnErr $ "at token : " ++ (show $ tokenString parseErrorToken)
      exitFailure
    renameError :: RenameError -> IO ()
    renameError RenameError {..} = do
      putStrLnErr "renameError"
      putStrLnErr renameErrorMsg
      putStrLnErr $ show renameErrorLoc
      exitFailure
    ioExc :: IOException -> IO ()
    ioExc err = do
      putStrLnErr $ show err
      exitFailure
    errCall :: ErrorCall -> IO ()
    errCall err = flip catches allHandler $ do
      putStrLnErr "unexpected error call"
      putStrLnErr $ show err
      exitFailure
    async :: AsyncException -> IO ()
    async err = do
      putStrLnErr "AsyncException (Pressing CRTL-C ?)"
      putStrLnErr $ show err
      exitFailure
    someExc :: SomeException -> IO ()
    someExc err = do
      putStrLnErr $ show err
      exitFailure
