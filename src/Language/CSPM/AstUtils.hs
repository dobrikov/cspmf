-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.AstUtils
-- Copyright   :  (c) Fontaine 2008 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Some utility functions for converting the AST

{-# LANGUAGE RankNTypes #-}
module Language.CSPM.AstUtils
  (
   removeSourceLocations
  ,removeParens
  ,removeModuleTokens
  ,unUniqueIdent
  ,showAst
  ,computeFreeNames
  ,getModuleAsserts
  ,annulNodeId
  )
where

import Language.CSPM.AST hiding (prologMode)
import qualified Language.CSPM.SrcLoc as SrcLoc

import qualified Data.IntMap as IntMap
import Data.Data
import Data.Maybe
import Data.Generics.Schemes (everywhere,listify)
import Data.Generics.Aliases (mkT,extQ)
--import Data.Generics.Basics (gmapQ,toConstr,showConstr)

-- | 'removeSourceLocations' sets all locationsInfos to 'NoLocation'
removeSourceLocations :: Data a => Labeled (Module a) -> Labeled (Module a)
removeSourceLocations ast 
  = everywhere (mkT patchLabel) ast
  where
    patchLabel :: SrcLoc.SrcLoc -> SrcLoc.SrcLoc
    patchLabel _ = SrcLoc.NoLocation

-- | set the tokenlist in the module datatype to Nothing
removeModuleTokens :: Labeled (Module a) -> Labeled (Module a)
removeModuleTokens t = t {unLabel = m}
  where m = (unLabel t ) {moduleTokens = Nothing}

-- | 'removeParens' removes all occurences of of Parens,i.e. explicit parentheses from the AST
removeParens :: Data a => Labeled (Module a) -> Labeled (Module a)
removeParens ast 
  = everywhere (mkT patchExp) ast
  where
    patchExp :: LExp -> LExp
    patchExp x = case unLabel x of
      Parens e -> e
      _ -> x

annulNodeId :: Data a => Labeled (Module a) -> Labeled (Module a)
annulNodeId ast 
  = everywhere (mkT nID) ast
  where
    nID :: NodeId -> NodeId
    nID _ = NodeId { unNodeId = 0 }
-- | unUniqueIdent replaces the all UIdent with the Ident of the the new name, thus forgetting
-- | additional information like the bindingside, etc.
-- | Usefull to get a smaller AST. 
unUniqueIdent :: Data a => Labeled (Module a) -> Labeled (Module a)
unUniqueIdent ast
  = everywhere (mkT patchIdent) ast
  where
    patchIdent :: Ident -> Ident
    patchIdent (UIdent u) = Ident $ newName u
    patchIdent _ = error "unUniqueIdent : did not expect and 'Ident' in the AST"

-- | 'a show function that omits the node labeles.
-- | TODO : fix this is very buggy.
-- | this does not work for Compiles pattern / Arrays
showAst :: Data a => Labeled a -> String
showAst ast = gshow ast
  where
    gshow :: Data a => a -> String
    gshow = mShow `extQ` (show :: String -> String)
    mShow t = if (tyConString $ typeRepTyCon $ typeOf t) == "Language.CSPM.AST.Labeled"
       then gmapQi 2 gshow t
       else
          "("
       ++ showConstr (toConstr t)
       ++ concat (gmapQ ((++) " " . gshow) t)
       ++ ")"

-- | Compute the "FreeNames" of an Expression.
-- | This function does only work after renaming has been done.
-- | This implementation is inefficient.
computeFreeNames :: Data a => a -> FreeNames
computeFreeNames syntax
  = IntMap.difference (IntMap.fromList used) (IntMap.fromList def)
  where
    used :: [(Int, UniqueIdent)]
    used = map (getIdent . unUse) $ listify isUse syntax
    def :: [(Int, UniqueIdent)]
    def  =  (map (getIdent . unDef) $ listify isDef syntax)
         ++ (map (getIdent . unDecl) $ listify isDecl syntax)
    getIdent :: LIdent -> (Int, UniqueIdent)
    getIdent x = (uniqueIdentId h, h)
      where h = unUIdent $ unLabel x

    isUse :: Exp -> Bool
    isUse (Var {}) = True
    isUse _ = False

    unUse (Var x) = x
    unUse _ = error "computeFreeNames : expecting Var"

    isDef :: Pattern -> Bool
    isDef (VarPat {}) = True
    isDef _ = False

    isDecl (FunBind {}) = True
    isDecl _ = False

    unDef (VarPat x) = x
    unDef _ = error "computeFreeNames : expecting VarPar"

    unDecl (FunBind x _) = x
    unDecl _ = error "computeFreeNames : expecting FunBind"

-- | Get the assert declarations of a module
getModuleAsserts :: Module a -> [LAssertDecl]
getModuleAsserts = mapMaybe justAssert . moduleDecls
  where
    justAssert decl = case unLabel decl of
      Assert  a -> Just a
      _ -> Nothing
