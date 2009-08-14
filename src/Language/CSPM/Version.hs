-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Version
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Use Template Haskell to gather some info a compile-time.
-- (Template-Haskell makes problems with haddock)
{-# LANGUAGE TemplateHaskell #-}
module Language.CSPM.Version
(
 version
)
where

import qualified Paths_CSPM_Frontend as Paths
import Language.Haskell.TH
import System.Time
import System.Info
import Data.List
--import Network.BSD  --check if this is the reason why we have to link to network ?
import Data.Version

-- | "version" returns a "String" with some info about the, on which the libray was built.
version :: IO String
version = return $( let 
    mkVersion :: IO String
    mkVersion = do
      timeDate <- getClockTime
--  hn <- getHostName
      let sysInfo = concat $ intersperse " " [
           "CSPM-Fronted"
           ,show Paths.version
           ,"\nCompiled at",show timeDate
--         ,"\non",hn
           ,"\n(",os,arch,compilerName,showVersion compilerVersion ,")"
           ]
      putStrLn "\n\n"
      putStrLn "version :"
      putStrLn sysInfo
      putStrLn "\n\n"
      return sysInfo
  in stringE =<< runIO mkVersion 
   )
