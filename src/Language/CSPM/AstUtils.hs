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
  ,relabelAst
  )
where

import Language.CSPM.AST hiding (prologMode)
import qualified Language.CSPM.AST as AST
import qualified Language.CSPM.SrcLoc as SrcLoc

import qualified Data.Generics.Schemes (everywhere)
import qualified Data.Generics.Aliases (mkT,ext1T)

-- | 'removeSourceLocations' sets all locationsInfos to 'NoLocation'
removeSourceLocations :: LModule  -> LModule  
removeSourceLocations ast 
  = Data.Generics.Schemes.everywhere (Data.Generics.Aliases.ext1T id patchLabel) ast
  where
    patchLabel :: Labeled t -> Labeled t
    patchLabel l = l {srcLoc = SrcLoc.NoLocation}

-- | 'removeParens' removes all occurences of of Parens,i.e. explicit parentheses from the AST
removeParens :: LModule  -> LModule  
removeParens ast 
  = Data.Generics.Schemes.everywhere (Data.Generics.Aliases.mkT patchExp) ast
  where
    patchExp :: LExp -> LExp
    patchExp x = case unLabel x of
      Parens e -> e
      _ -> x

-- | 'relabelAst' compute an AST with new NodeIds starting with the given NodeId
relabelAst :: 
     NodeId 
  -> LModule 
  -> LModule
relabelAst = error "relabel not yet implemented (TODO)"