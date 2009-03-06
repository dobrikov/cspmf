{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.SrcLoc
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  provisional
-- Portability :  GHC-only
--
-- This module contains the datatype for sourcelocations and some utility functions.

module Language.CSPM.SrcLoc
where

import Language.CSPM.Token as Token

import Data.List
import Data.Typeable (Typeable)
import Data.Generics.Basics (Data)
import Data.Generics.Instances ()

{-  todo : simplify this -}
data SrcLoc
  = TokIdPos TokenId
  | TokIdSpan TokenId TokenId
  | TokSpan Token Token -- the spans are closed intervals
                        -- single token with token x :: TokSpan x x
  | TokPos Token
  | NoLocation
  | FixedLoc {
      fixedStartLine   :: !Int
     ,fixedStartCol    :: !Int
     ,fixedStartOffset :: !Int
     ,fixedLen         :: !Int
     ,fixedEndLine     :: !Int
     ,fixedEndCol      :: !Int
     ,fixedEndOffset   :: !Int
   }

  deriving (Show,Eq,Ord,Typeable, Data)

type SrcLine = Int
type SrcCol  = Int
type SrcOffset  = Int

getStartLine :: SrcLoc -> SrcLine
getStartLine x = case x of
  TokSpan s _e -> alexLine $ tokenStart s
  TokPos t     -> alexLine $ tokenStart t
  FixedLoc {}  -> fixedStartLine x
  _ -> error "no SrcLine Availabel"

getStartCol :: SrcLoc -> SrcCol
getStartCol x = case x of
  TokSpan s _e -> alexCol $ tokenStart s
  TokPos t     -> alexCol $ tokenStart t
  FixedLoc {}  -> fixedStartCol x
  _ -> error "no SrcCol Availabel"

getStartOffset :: SrcLoc -> SrcOffset
getStartOffset x = case x of
  TokSpan s _e -> alexPos $ tokenStart s
  TokPos t     -> alexPos $ tokenStart t
  FixedLoc {}  -> fixedStartOffset x
  _ ->  error "no SrcOffset Availabel"

getTokenLen :: SrcLoc -> SrcOffset
getTokenLen x = case x of
  TokPos t -> tokenLen t
  TokSpan s e -> (alexPos $ tokenStart e) - (alexPos $ tokenStart s) + tokenLen e
  FixedLoc {}  -> fixedLen x
  _ -> error "getTokenLen : info not Available"

getEndLine :: SrcLoc -> SrcLine
getEndLine x = case x of
  TokSpan _s e -> alexLine $ computeEndPos e
  TokPos t -> alexLine $ computeEndPos t
  FixedLoc {}  -> fixedEndLine x
  _ ->   error "no SrcLine Availabel"

getEndCol :: SrcLoc -> SrcCol
getEndCol x = case x of
  TokSpan _s e -> alexCol $ computeEndPos e
  TokPos t -> alexCol $ computeEndPos t
  FixedLoc {}  -> fixedEndCol x
  _ ->  error "no SrcCol Availabel"

getEndOffset :: SrcLoc -> SrcOffset
getEndOffset x = case x of
  TokSpan _s e -> (alexPos $ tokenStart e) + tokenLen e
  TokPos t -> (alexPos $ tokenStart t) + tokenLen t
  FixedLoc {}  -> fixedEndOffset x
  _ ->  error "no SrcOffset Availabel"

computeEndPos :: Token -> AlexPosn
computeEndPos t = foldl' alexMove (tokenStart t) (tokenString t)

getTokenId :: SrcLoc -> TokenId
getTokenId (TokIdPos x) = x
getTokenId (TokIdSpan x _) = x
getTokenId (TokSpan x _ ) = Token.tokenId x
getTokenId (TokPos x) = Token.tokenId x
getTokenId _ = error "no SrcOffset Availabel"

{-# DEPRECATED srcLocFromTo "sourceLoc arithemtics is not reliable" #-}
srcLocFromTo :: SrcLoc -> SrcLoc -> SrcLoc
srcLocFromTo s e = FixedLoc {
   fixedStartLine   = getStartLine s
  ,fixedStartCol    = getStartCol s
  ,fixedStartOffset = getStartOffset s
  ,fixedLen         = getStartOffset e
                       - getStartOffset s
                       - getTokenLen s
  ,fixedEndLine     = getEndLine e
  ,fixedEndCol      = getEndCol e
  ,fixedEndOffset   = getEndOffset e
   }

{-# DEPRECATED srcLocBetween "sourceLoc arithemtics is not reliable" #-}
srcLocBetween :: SrcLoc -> SrcLoc -> SrcLoc
srcLocBetween s e = FixedLoc {
   fixedStartLine   = getEndLine s
  ,fixedStartCol    = getEndCol s + 1     -- maybe wrong when token at end of Line
  ,fixedStartOffset = getStartOffset s + getTokenLen e
  ,fixedLen         = getStartOffset e
                      - getStartOffset s
                      - getTokenLen s
  ,fixedEndLine     = getStartLine e
  ,fixedEndCol      = getStartCol e -1    -- maybe wrong when startCol = 0
  ,fixedEndOffset   = getStartOffset e
   }
