{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
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
import GHC.Generics (Generic)
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
  deriving (Show,Eq,Ord,Typeable, Data, Generic)

mkTokSpan :: Token -> Token -> SrcLoc
mkTokSpan = TokSpan

mkTokPos :: Token -> SrcLoc
mkTokPos = TokPos

type SrcLine = Int
type SrcCol  = Int
type SrcOffset  = Int

getStartLine :: SrcLoc -> SrcLine
getStartLine x = case x of
  TokSpan s _e  -> alexLine $ tokenStart s
  TokPos t      -> alexLine $ tokenStart t
  FixedLoc {}   -> fixedStartLine x
  _ -> error "no SrcLine Availabel"

getStartCol :: SrcLoc -> SrcCol
getStartCol x = case x of
  TokSpan s _e  -> alexCol $ tokenStart s
  TokPos t      -> alexCol $ tokenStart t
  FixedLoc {}   -> fixedStartCol x
  _ -> error "no SrcCol Availabel"

getStartOffset :: SrcLoc -> SrcOffset
getStartOffset x = case x of
  TokSpan s _e  -> alexPos $ tokenStart s
  TokPos t      -> alexPos $ tokenStart t
  FixedLoc {}   -> fixedStartOffset x
  _ ->  error "no SrcOffset available"

getTokenLen :: SrcLoc -> SrcOffset
getTokenLen x = case x of
  TokPos t -> tokenLen t
  TokSpan s e   -> (alexPos $ tokenStart e) - (alexPos $ tokenStart s) + tokenLen e
  FixedLoc {}  -> fixedLen x
  _ -> error "getTokenLen : info not available"

getEndLine :: SrcLoc -> SrcLine
getEndLine x = case x of
  TokSpan _s e  -> alexLine $ computeEndPos e
  TokPos t -> alexLine $ computeEndPos t
  FixedLoc {}  -> fixedEndLine x
  _ ->   error "no SrcLine available"

getEndCol :: SrcLoc -> SrcCol
getEndCol x = case x of
  TokSpan _s e  -> alexCol $ computeEndPos e
  TokPos t -> alexCol $ computeEndPos t
  FixedLoc {}  -> fixedEndCol x
  _ ->  error "no SrcCol available"

getEndOffset :: SrcLoc -> SrcOffset
getEndOffset x = case x of
  TokSpan _s e  -> (alexPos $ tokenStart e) + tokenLen e
  TokPos t -> (alexPos $ tokenStart t) + tokenLen t
  FixedLoc {}  -> fixedEndOffset x
  _ ->  error "no SrcOffset available"

getStartTokenId :: SrcLoc -> TokenId
getStartTokenId s = case s of
  TokIdPos x -> x
  TokIdSpan x _ -> x
  TokSpan x _   -> Token.tokenId x
  TokPos x -> Token.tokenId x
  _ -> error "no startTokenId available"

getEndTokenId :: SrcLoc -> TokenId
getEndTokenId s = case s of
  TokIdPos x -> x
  TokIdSpan _ x -> x
  TokSpan _ x   -> Token.tokenId x
  TokPos x -> Token.tokenId x
  _ -> error "no endTokenId available"

getStartToken :: SrcLoc -> Token
getStartToken s = case s of
  TokSpan x _   -> x
  TokPos x -> x
  _ -> error "SrcLoc: no startToken available"

getEndToken :: SrcLoc -> Token
getEndToken s = case s of
  TokSpan _ x   -> x
  TokPos x -> x
  _ -> error "SrcLoc: no endToken available"


computeEndPos :: Token -> AlexPosn
computeEndPos t = foldl' alexMove (tokenStart t) (tokenString t)


{-# DEPRECATED srcLocFromTo "sourceLoc arithmetics is not reliable" #-}
-- this is the closed Interval between s and e
srcLocFromTo :: SrcLoc -> SrcLoc -> SrcLoc
srcLocFromTo NoLocation _ = NoLocation
srcLocFromTo _ NoLocation = NoLocation
srcLocFromTo (TokSpan s _) (TokSpan _ e) = TokSpan s e
srcLocFromTo s e = FixedLoc {
   fixedStartLine   = getStartLine s
  ,fixedStartCol    = getStartCol s
  ,fixedStartOffset = getStartOffset s
  ,fixedLen         = getEndOffset e - getStartOffset s
  ,fixedEndLine     = getEndLine e
  ,fixedEndCol      = getEndCol e
  ,fixedEndOffset   = getEndOffset e
   }

{-# DEPRECATED srcLocBetween "sourceLoc arithmetics is not reliable" #-}
-- this is the open Interval between s and e
srcLocBetween :: SrcLoc -> SrcLoc -> SrcLoc
srcLocBetween NoLocation _ = NoLocation
srcLocBetween _ NoLocation = NoLocation
srcLocBetween s e = FixedLoc {
   fixedStartLine   = getEndLine s
  ,fixedStartCol    = getEndCol s + 1     -- maybe wrong when token at end of Line
  ,fixedStartOffset = getStartOffset s + getTokenLen s
  ,fixedLen         = getEndOffset e - getStartOffset s
  ,fixedEndLine     = getStartLine e
  ,fixedEndCol      = getStartCol e -1    -- maybe wrong when startCol = 0
  ,fixedEndOffset   = getStartOffset e
   }
