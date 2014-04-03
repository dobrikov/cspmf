{-# LANGUAGE DeriveDataTypeable #-}
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
-- Reorder bindings for a call by value strategy.
-- ToDo: think if this is correct.
-- to be safe, we must also rule out multiple recursive functions ?
-- ToDo: add support for channel delcs and other top-level decls

module Language.CSPM.ReorderBindings
  (
  reorderModule
  )
where

import Language.CSPM.AST hiding (prologMode)
import qualified Language.CSPM.AST as AST
import qualified Language.CSPM.SrcLoc as SrcLoc

import Data.Data
import Data.Generics.Schemes (everything,everywhereM)
import Data.Generics.Aliases (mkM,extQ,mkQ)
import Data.Generics.Basics (gmapQ,toConstr,showConstr)
import Control.Exception (Exception)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Error

data BindError
  = BindError
  {
   bindErrorMsg :: String
  ,bindErrorLoc :: SrcLoc.SrcLoc
  } deriving (Show,Typeable)

instance Exception BindError
instance Error BindError where
  noMsg    = BindError "unknown" SrcLoc.NoLocation
  strMsg x = BindError x SrcLoc.NoLocation

type RM x = Either BindError x

-- | reorderModule expects an AST that has gone through renaming
reorderModule :: LModule -> RM LModule
reorderModule ast = do
  a2 <- everywhereM (mkM fixLet) ast
  reorderTopLevel a2
  where
    fixLet :: LExp -> RM LExp
    fixLet node = case unLabel node of
       Let decls e -> do
          d <- reorderGroup decls
          return $ setNode node $ Let d e
       _ -> return node
         
    reorderTopLevel :: LModule -> RM LModule
    reorderTopLevel = error "reorder Toplevel"

reorderGroup :: [LDecl] -> RM [LDecl]
reorderGroup l = do
  let d = map (\x -> (x, defs x, uses x)) l
  return $ error "reorderGroup"

-- | Compute the set of Identifier that get defined 
-- | Todo: unit-tests for this
defs :: LDecl -> Set UniqueIdent
defs d = case unLabel d of
  PatBind lpat _ -> everything Set.union ( Set.empty `mkQ` fromPattern) lpat
  FunBind i _ -> Set.singleton $ unUIdent $ unLabel i
  _ -> Set.empty  -- todo : add missing cases for top-level decls
  where
    fromPattern :: Pattern -> Set UniqueIdent
    fromPattern (VarPat i) = Set.singleton $ unUIdent $ unLabel i
    fromPattern _ = Set.empty

-- | Uses is NOT the free names analysis.
-- | We omit everything inside prefix-operations. (ToDO)
-- | Todo: think if this is correkt
uses :: LDecl -> Set UniqueIdent
uses d = case unLabel d of
  PatBind _ e -> collectVars e
  FunBind _ cases -> collectVars cases
  _ -> Set.empty  -- todo : add missing cases for top-level decls
  where
    collectVars :: Data t => t -> Set UniqueIdent
    collectVars = everything Set.union ( Set.empty `mkQ` fromExpr)
-- | We omit everything inside prefix-operations. (ToDO)
    fromExpr :: Exp -> Set UniqueIdent
    fromExpr (Var i) = Set.singleton $ unUIdent $ unLabel i
    fromExpr _ = Set.empty