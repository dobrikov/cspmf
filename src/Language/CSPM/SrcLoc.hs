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
-- This module contains the datatype for Sourcelocations and some utility functions.

module Language.CSPM.SrcLoc
where

import Language.CSPM.Token as Token

import Control.Monad
import Data.Typeable (Typeable)
import Data.Generics.Basics (Data)
import Data.Generics.Instances ()

data SrcLoc
  = TokIdPos TokenId
  | TokIdSpan TokenId TokenId
  | TokSpan Token Token -- the spans are closed intervals
                        -- single token with token x :: TokSpan x x
  | TokPos Token
  | NoLocation
  deriving (Show,Eq,Ord,Typeable, Data)

type SrcLine = Int
type SrcCol  = Int
type SrcOffset  = Int



getStartLine :: SrcLoc -> SrcLine
getStartLine (TokSpan s _e) = alexLine $ tokenStart s
getStartLine (TokPos t) = alexLine $ tokenStart t
getStartLine _ = error "no SrcLine Availabel"

getStartCol :: SrcLoc -> SrcCol
getStartCol (TokSpan s _e) = alexCol $ tokenStart s
getStartCol (TokPos t) = alexCol $ tokenStart t
getStartCol _ = error "no SrcCol Availabel"

getStartOffset :: SrcLoc -> SrcOffset
getStartOffset (TokSpan s _e) = alexPos $ tokenStart s
getStartOffset (TokPos t) = alexPos $ tokenStart t
getStartOffset _ =  error "no SrcOffset Availabel"

getTokenLen :: SrcLoc -> SrcOffset
getTokenLen (TokPos t) = tokenLen t
getTokenLen (TokSpan s e)
  = (alexPos $ tokenStart e) - (alexPos $ tokenStart s) + tokenLen e
getTokenLen _ = error "getTokenLen : info not Available"

{-
fix this !!
-}

getEndLine :: SrcLoc -> SrcLine
getEndLine (TokSpan _s e) = alexLine $ tokenStart e
getEndLine (TokPos t) = alexLine $ tokenStart t
getEndLine _ =  error "no SrcLine Availabel"

getEndCol :: SrcLoc -> SrcCol
getEndCol (TokSpan _s e) = alexCol $ tokenStart e
getEndCol (TokPos t) = alexCol $ tokenStart t
getEndCol _ =  error "no SrcCol Availabel"

getEndOffset :: SrcLoc -> SrcOffset
getEndOffset (TokSpan _s e) = alexPos $ tokenStart e
getEndOffset (TokPos t) = alexPos $ tokenStart t
getEndOffset _ =  error "no SrcOffset Availabel"

getTokenId :: SrcLoc -> TokenId
getTokenId (TokIdPos x) = x
getTokenId (TokIdSpan x _) = x
getTokenId (TokSpan x _ ) = Token.tokenId x
getTokenId (TokPos x) = Token.tokenId x
getTokenId _ = error "no SrcOffset Availabel"


{-
getStartLine :: (MonadPlus m) => SrcLoc -> SrcLine
getStartLine (TokSpan s _e) = return $ alexLine $ tokenStart s
getStartLine (TokPos t) = return $ alexLine $ tokenStart t
getStartLine _ = mzero

getStartCol :: (MonadPlus m) => SrcLoc -> SrcCol
getStartCol (TokSpan s _e) = return $ alexCol $ tokenStart s
getStartCol (TokPos t) = return $ alexCol $ tokenStart t
getStartCol _ = mzero

getStartOffset :: (MonadPlus m) => SrcLoc -> SrcOffset
getStartOffset (TokSpan s _e) = return $ alexPos $ tokenStart s
getStartOffset (TokPos t) = return $ alexPos $ tokenStart t
getStartOffset _ = mzero

getEndLine :: (MonadPlus m) => SrcLoc -> SrcLine
getEndLine (TokSpan _s e) = return $ alexLine $ tokenEnd e
getEndLine (TokPos t) = return $ alexLine $ tokenEnd t
getEndLine _ = mzero

getEndCol :: (MonadPlus m) => SrcLoc -> SrcCol
getEndCol (TokSpan _s e) = return $ alexCol $ tokenEnd e
getEndCol (TokPos t) = return $ alexCol $ tokenEnd t
getEndCol _ = mzero

getEndOffset :: (MonadPlus m) => SrcLoc -> SrcOffset
getEndOffset (TokSpan _s e) = return $ alexPos $ tokenEnd e
getEndOffset (TokPos t) = return $ alexPos $ tokenEnd t
getEndOffset _ = mzero
-}
class Monad m => HaveTokenList m where
  getToken :: TokenId -> m Token

{-
getEnd
getEndCol
getEndOffset
-}