-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.AstUtils
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Some utility functions for converting the AST

module Language.CSPM.AstUtils
  (
   removeSourceLocations
  ,removeParens
  ,removeModuleTokens
  ,unUniqueIdent
  ,showAst
  ,relabelAst
  ,computeFreeNames
  )
where

import Language.CSPM.AST hiding (prologMode)
import qualified Language.CSPM.AST as AST
import qualified Language.CSPM.SrcLoc as SrcLoc

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Data
import Data.Generics.Schemes (everywhere,listify)
import Data.Generics.Aliases (mkT,extQ)
import Data.Generics.Basics (gmapQ,toConstr,showConstr)

-- | 'removeSourceLocations' sets all locationsInfos to 'NoLocation'
removeSourceLocations :: LModule  -> LModule  
removeSourceLocations ast 
  = everywhere (mkT patchLabel) ast
  where
    patchLabel :: SrcLoc.SrcLoc -> SrcLoc.SrcLoc
    patchLabel _ = SrcLoc.NoLocation

-- | set the tokenlist in the module datatype to Nothing
removeModuleTokens :: LModule -> LModule
removeModuleTokens t = t {unLabel = m}
  where m = (unLabel t ) {moduleTokens = Nothing}

-- | 'removeParens' removes all occurences of of Parens,i.e. explicit parentheses from the AST
removeParens :: LModule  -> LModule  
removeParens ast 
  = everywhere (mkT patchExp) ast
  where
    patchExp :: LExp -> LExp
    patchExp x = case unLabel x of
      Parens e -> e
      _ -> x

-- | unUniqueIdent replaces the all UIdent with the Ident of the the new name, thus forgetting
-- | additional information like the bindingside, etc.
-- | Usefull to get a smaller AST. 
unUniqueIdent :: LModule  -> LModule  
unUniqueIdent ast
  = everywhere (mkT patchIdent) ast
  where
    patchIdent :: Ident -> Ident
    patchIdent (UIdent u) = Ident $ newName u
    patchIdent _ = error "unUniqueIdent : did not expect and 'Ident' in the AST"

-- | 'relabelAst' compute an AST with new NodeIds starting with the given NodeId
relabelAst :: 
     NodeId 
  -> LModule 
  -> LModule
relabelAst = error "relabel not yet implemented (TODO)"

-- | 'a show function that omits the node labeles
showAst :: LModule -> String
showAst ast = gshow ast
  where
    gshow :: Data a => a -> String
    gshow = mShow `extQ` (show :: String -> String)
    mShow t = if (tyConString $ typeRepTyCon $ typeOf t) == "Language.CSPM.AST.Labeled"
       then gmapQi 1 gshow t
       else
          "("
       ++ showConstr (toConstr t)
       ++ concat (gmapQ ((++) " " . gshow) t)
       ++ ")"



-- | Compute the "FreeNames" of an Expression.
-- | This function does only work after renaming has been done.
-- | This implementation is inefficient.
computeFreeNames :: LExp -> FreeNames
computeFreeNames expr
  = IntMap.difference (toIntMap used) (toIntMap def)
  where
    toIntMap :: [UniqueIdent] -> IntMap UniqueIdent
    toIntMap
      = IntMap.fromList . map (\x -> (uniqueIdentId x,x))
    used :: [UniqueIdent]
    used = map (\(Var x) -> unUIdent $ unLabel x) $ listify isUse expr
    def :: [UniqueIdent]
    def  = map (\(VarPat x) -> unUIdent $ unLabel x) $ listify isDef expr
    isUse :: Exp -> Bool
    isUse (Var {}) = True
    isUse _ = False

    isDef :: Pattern -> Bool
    isDef (VarPat {}) = True
    isDef _ = False