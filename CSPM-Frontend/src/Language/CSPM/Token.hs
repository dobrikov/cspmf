-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Token
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  provisional
-- Portability :  GHC-only
--
-- This module contains the data type Tokens and some helper functions

{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveGeneric #-}

module Language.CSPM.Token
where

import Language.CSPM.TokenClasses

import Data.Typeable (Typeable)
import Data.Generics.Basics (Data)
import GHC.Generics (Generic)
import Data.Generics.Instances ()
import Data.Ix
import Control.Exception (Exception)

newtype TokenId = TokenId {unTokenId :: Int}
  deriving (Show, Eq, Ord, Enum, Ix, Typeable, Data, Generic)

mkTokenId :: Int -> TokenId
mkTokenId = TokenId

data AlexPosn = AlexPn {
   alexPos :: !Int
  ,alexLine   :: !Int 
  ,alexCol    :: !Int
  } deriving (Show,Eq,Ord, Typeable, Data, Generic)

pprintAlexPosn :: AlexPosn -> String
pprintAlexPosn (AlexPn _p l c) = "Line: "++show l++" Col: "++show c

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l _c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


data LexError = LexError {
   lexEPos :: !AlexPosn
  ,lexEMsg :: !String
  } deriving (Show, Typeable)
instance Exception LexError


data Token = Token
  { tokenId     :: TokenId
  , tokenStart  :: AlexPosn
  , tokenLen    :: Int
  , tokenClass  :: PrimToken
  , tokenString :: String
  } deriving (Show, Eq, Ord, Typeable, Data, Generic)

tokenSentinel :: Token
tokenSentinel = Token
  { tokenId = mkTokenId (- 1)
  , tokenStart = AlexPn 0 0 0
  , tokenLen = 0
  , tokenClass  = error "CSPLexer.x illegal access tokenSentinel"
  , tokenString =error "CSPLexer.x illegal access tokenSentinel"}

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = show line ++ ':': show col

showToken :: Token -> String
showToken Token {tokenString = str} = "'" ++ str ++ "'"
