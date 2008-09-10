{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Language.CSPM.Token
where

import Data.Typeable (Typeable)
import Data.Generics.Basics (Data)
import Data.Generics.Instances ()
import Data.Ix
import Data.Char

newtype TokenId = TokenId {unTokenId :: Int}
  deriving (Show,Eq,Ord,Enum,Ix, Typeable, Data)

mkTokenId :: Int -> TokenId
mkTokenId = TokenId



data AlexPosn = AlexPn !Int !Int !Int
  deriving (Show,Eq,Ord, Typeable, Data)

alexLine (AlexPn _ l _) =l
alexCol (AlexPn _ _ c) =c
alexPos (AlexPn p _ _) =p

pprintAlexPosn (AlexPn p l c) = "Line: "++show l++" Col: "++show c

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

data LexError = LexError {
   lexEPos :: !AlexPosn
  ,lexEMsg :: !String
  } deriving (Show, Typeable)


data Token = Token
  { tokenId     :: TokenId
  , tokenStart  :: AlexPosn
  , tokenLen    :: Int
  , tokenClass  :: TokenClass
  , tokenString :: String
  } deriving (Show,Eq,Ord, Typeable, Data)

tokenSentinel = Token
  { tokenId = mkTokenId (- 1)
  , tokenStart = AlexPn 0 0 0
  , tokenLen = 0
  , tokenClass  =error "CSPLexer.x illegal access tokenSentinel"
  , tokenString =error "CSPLexer.x illegal access tokenSentinel"}



data TokenClass
  = LInteger
--  | LFloat
--  | LChar
  | LString
  | LCspId
  | LCspBI
  | LCspsym
  | LIdent
  | LCSPFDR
  | LLComment
  | LBComment
  | LEOF
  | LInclude
  deriving (Show,Eq,Ord,Enum,Ix, Typeable, Data)

showPosn (AlexPn _ line col) = show line ++ ':': show col
showToken Token {tokenString=str} = "'"++str++"'"

