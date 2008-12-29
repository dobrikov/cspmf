-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.PatternCompiler
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- replace nested patterns with a set of linear Selectors
-- todo : benchmark if it pays off to introduce 
-- helperbindings and only atomic bindings (unlikely)
-- todo : add testcases

module Language.CSPM.PatternCompiler
  (
  compilePattern
  )
where

import Language.CSPM.AST hiding (prologMode)
import qualified Language.CSPM.AST as AST

import Control.Monad
import Data.Generics.Schemes (everywhere')
import Data.Generics.Aliases (mkT)
import Data.Array.IArray

-- | replace all pattern in the module with list of linear Selectors
compilePattern :: LModule -> LModule
compilePattern ast 
  = Data.Generics.Schemes.everywhere' (Data.Generics.Aliases.mkT compPat)
      $ replaceFunCase ast -- temporay patch
  where
    compPat :: LPattern -> LPattern
    compPat pat = let
       p :: [(Maybe LIdent,Selector)]
       p = cp id pat
       len = length sels
       sels = map snd p
       ids  = map fst p
       lToArr l = array (0,len-1) $ zip [0..] l
      in 
--todo special case len==1
      setNode pat $ Selectors {
 -- fixme this creates an infinite tree with SYB everywehre'
--        origPat = pat
        selectors = lToArr sels
       ,idents = lToArr ids
       }
   

    cp :: (Selector -> Selector ) -> LPattern -> [(Maybe LIdent,Selector)]
    cp path pat = case unLabel pat of
      IntPat i -> return (Nothing, path $ IntSel i)
      TruePat  -> return (Nothing, path TrueSel )
      FalsePat -> return (Nothing, path FalseSel )
      WildCard -> return (Nothing, path SelectThis )
      VarPat x -> return (Just x , path SelectThis )
{-      | Also [LPattern]
      | Append [LPattern]
      | DotPat [LPattern] -}
      SingleSetPat p -> cp (path . SingleSetSel) p
      EmptySetPat -> return (Nothing, path EmptySetSel)
      ListEnumPat [] -> return (Nothing, path $ ListLengthSel 0 $ SelectThis )
      ListEnumPat l -> do
        let len = length l   
        msum $ map 
          (\(x,i) -> cp (path . ListLengthSel len . ListIthSel i) x)
          (zip l [0..])
      TuplePat [] -> return (Nothing, path $ TupleLengthSel 0 $ SelectThis )
      TuplePat l -> do
        let len = length l
        msum $ map
          (\(x,i) -> cp (path . TupleLengthSel len . TupleIthSel i) x)
          (zip l [0..])


{- replaceFunCase with funCaseNew -}
replaceFunCase :: LModule -> LModule
replaceFunCase ast
  = Data.Generics.Schemes.everywhere' (Data.Generics.Aliases.mkT compFC) ast
  where
    compFC :: FunCase -> FunCase
    compFC (FunCase args expr) = FunCaseNew flatArgs expr
      where
        flatArgs = map wrapTuple args
        wrapTuple [a] = a  -- one-element lists are not Tuples ?
        wrapTuple x   =(AST.labeled . TuplePat) x
    compFC (FunCaseNew _ _) = error "Parser does not generate FunCaseNew"
