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

    mkListPrefixPat 
      :: (Selector -> Selector ) 
          -> (Offset,Len,LPattern)
          -> [(Maybe LIdent,Selector)]
    mkListPrefixPat path l = case l of
      (0,1,pat) -> let (unLabel -> ListEnumPat [r]) = pat
                   in cp (path . HeadSel) r
      (0,n,pat) -> cp (path . HeadNSel n) pat
      (o,l,pat) -> cp (path . PrefixSel o l) pat

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
    mkListVariablePat path Nothing = []
    mkListVariablePat path (Just (l,r,pat)) = cp (path . SliceSel l r) pat


{-
replaceFunCase with funCaseNew 
this is a quickfix
in CSPM we have tree cases:
fun(x)(y) fun(x,y) and fun((x,y))
we want to map them to fun x y and fun (x,y)
-}
replaceFunCase :: LModule -> LModule
replaceFunCase ast
  = Data.Generics.Schemes.everywhere' (Data.Generics.Aliases.mkT compFC) ast
  where
    compFC :: FunCase -> FunCase
    compFC (FunCase args expr) = FunCaseNew flatArgs expr
      where
        flatArgs = case args of
          [x] -> x
          _     -> map wrapTuple args
        wrapTuple [a] = a  -- one-element lists are not Tuples ?
        wrapTuple x   =(AST.labeled . TuplePat) x
    compFC (FunCaseNew _ _) = error "Did not expect FunCaseNew in parse Result"

type Offset = Int
type Len    = Int
analyzeAppendPattern :: 
  [LPattern] ->
    ([(Offset,Len,LPattern)] -- | prefixpattern
    ,[(Offset,Len,LPattern)] -- | suffixpattern
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
      Append pl -> do
        l <- mapM lengthOfListPattern pl
        return $ sum l
      VarPat _ -> Nothing
      Also pl -> do
        let l = map lengthOfListPattern pl
        error "PatternCompiler.hs: lengthOfListPat : alsopattern: todo"
      p -> error $ "PatternCompiler.hs: lengthOfListPat : no list pattern "
                    ++ show p

-- | PrefixPattern are fixed-length pattern with a fixed offset from the front
-- | return when the first variable length pattern occurs

    computePrefixPattern :: [(LPattern,Maybe Len)] -> [(Offset,Len,LPattern)]
    computePrefixPattern l = worker 0 l where
      worker _      [] = []
      worker _      ((_ ,Nothing ) : _) = [] 
      worker offset ((pat,Just len): rest) 
        = (offset,len,pat) : worker (offset+len) rest

    computeSuffixPattern :: [(LPattern,Maybe Len)] -> [(Offset,Len,LPattern)]
    computeSuffixPattern = computePrefixPattern . reverse