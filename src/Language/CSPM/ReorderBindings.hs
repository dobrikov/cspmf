-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.ReorderBindings
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Rule out multiple recursive bindinggroups.
-- Reorder bindings for call by value strategy.
-- 

module Language.CSPM.ReorderBindings
  (
  reorderModule
  )
where

import Language.CSPM.AST hiding (prologMode)
import qualified Language.CSPM.AST as AST
import qualified Language.CSPM.SrcLoc as SrcLoc

import Data.Data
import Data.Generics.Schemes (everywhereM)
import Data.Generics.Aliases (mkT,extQ)
import Data.Generics.Basics (gmapQ,toConstr,showConstr)

data BindError
  = BindError
  {
   bindErrorMsg :: String
  ,bindErrorLoc :: SrcLoc.SrcLoc
  } deriving (Show,Typeable)

instance Exception RenameError

type RM x = Either BindError x

- | reorderModule expects an AST that has gone through renaming
reorderModule :: LModule -> RM LModule
reorderModule ast = reorderRecursive ast >>= reorderTopLevel
  where
    reorderRecursive :: LModule -> RM LModule
    reorderRecursive = everywhereM (mkT fixLet) ast
    fixLet :: LExp -> RM LExp
    fixLet node = case unlabel node of
       Let decls e -> do
          d <- reorderGroup decls
          return $ setNode node $ Let d e
       _ -> return e 
         
