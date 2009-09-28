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
{-# LANGUAGE ViewPatterns #-}

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
  = Data.Generics.Schemes.everywhere' (Data.Generics.Aliases.mkT compPat) ast
  where
-- pattern that consist only of a variable match remain unchanged
    compPat :: LPattern -> LPattern
    compPat x@(unLabel -> VarPat {}) = x
    compPat pat = case cp id pat of
       [(i,s)] -> setNode pat $ Selector s i
       x -> setNode pat $ Selectors {
--        origPat = pat
                    selectors = listToArr $ map snd x
                   ,idents = listToArr $ map fst x
                   }

    listToArr :: [a] -> Array Int a
    listToArr l = array (0,length l -1) $ zip [0..] l

    cp :: (Selector -> Selector ) -> LPattern -> [(Maybe LIdent,Selector)]
    cp path pat = case unLabel pat of
      IntPat i -> return (Nothing, path $ IntSel i)
      TruePat  -> return (Nothing, path TrueSel )
      FalsePat -> return (Nothing, path FalseSel )
      WildCard -> return (Nothing, path SelectThis )
      VarPat x -> return (Just x , path SelectThis ) 
      ConstrPat x -> return (Nothing, path $ ConstrSel $ unUIdent $ unLabel x)
{-      Also l ->  -}
      Append l -> do
        let (prefix,suffix,variable) = analyzeAppendPattern l
        msum [ concatMap (mkListPrefixPat path) prefix
             , mkListVariablePat path variable
             , concatMap (mkListSuffixPat path) suffix ]
{-      | DotPat [LPattern] -}
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
      Selector {} -> error "PatternCompiler.hs : didn't expect Selector"
      Selectors {} -> error "PatternCompiler.hs : didn't expect Selectors"

    mkListPrefixPat 
      :: (Selector -> Selector ) 
          -> (Offset,Len,LPattern)
          -> [(Maybe LIdent,Selector)]
    mkListPrefixPat path l = case l of
      (0,1,pat) -> let (unLabel -> ListEnumPat [r]) = pat
                   in cp (path . HeadSel) r
      (0,n,pat) -> cp (path . HeadNSel n) pat
      (o,s,pat) -> cp (path . PrefixSel o s) pat

    mkListSuffixPat 
      :: (Selector -> Selector ) 
          -> (Offset,Len,LPattern)
          -> [(Maybe LIdent,Selector)]
    mkListSuffixPat path (o,l,pat)
      = cp (path . SuffixSel o l) pat

    mkListVariablePat
      :: (Selector -> Selector ) 
          -> Maybe (Offset,Offset,LPattern)
          -> [(Maybe LIdent,Selector)]
    mkListVariablePat _path Nothing = []
    mkListVariablePat path (Just (l,r,pat)) = cp (path . SliceSel l r) pat

type Offset = Int
type Len    = Int
analyzeAppendPattern :: 
  [LPattern] ->
    ([(Offset,Len,LPattern)] --  prefixpattern
    ,[(Offset,Len,LPattern)] --  suffixpattern
    ,Maybe (Offset,Offset,LPattern)
    )
analyzeAppendPattern pl
  = let
    taggedPatList = zip pl $ map lengthOfListPattern pl
    prefixPat = computePrefixPattern taggedPatList
    suffixPat = computeSuffixPattern taggedPatList
    lenPrefix = sum $ map (\(_,l,_)-> l) prefixPat
    lenSuffix = sum $ map (\(_,l,_)-> l) suffixPat
    varPat = case filter (\(_,len) -> len == Nothing) taggedPatList of
              [] -> Nothing
              [(pat,_)] -> Just (lenPrefix,lenSuffix,pat)
              l -> error $ "PatternCompiler.hs : alsopattern contains multiple "
                   ++ "variable length pattern "
                   ++ show l
  in 
    (prefixPat,suffixPat,varPat)
  where
{-  compute the length of a list pattern
    Just Int -> fixed length pattern
    Nothing -> variable length pattern -}

    lengthOfListPattern :: LPattern -> Maybe Len
    lengthOfListPattern p = case unLabel p of
      ListEnumPat l -> return $ length l
      Append patl -> do
        l <- mapM lengthOfListPattern patl
        return $ sum l
      VarPat _ -> Nothing
      Also patl -> do
        let l = map lengthOfListPattern patl
        error "PatternCompiler.hs: lengthOfListPat : alsopattern: todo"
      _ -> error $ "PatternCompiler.hs: lengthOfListPat : no list pattern "
                    ++ show p
{-
PrefixPattern are fixed-length pattern with a fixed offset from the front
return when the first variable length pattern occurs
-}
    computePrefixPattern :: [(LPattern,Maybe Len)] -> [(Offset,Len,LPattern)]
    computePrefixPattern l = worker 0 l where
      worker _      [] = []
      worker _      ((_ ,Nothing ) : _) = [] 
      worker offset ((pat,Just len): rest) 
        = (offset,len,pat) : worker (offset+len) rest

    computeSuffixPattern :: [(LPattern,Maybe Len)] -> [(Offset,Len,LPattern)]
    computeSuffixPattern = computePrefixPattern . reverse