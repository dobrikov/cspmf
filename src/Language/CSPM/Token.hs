module Language.CSPM.Token
where

import Data.Ix
import Data.Typeable
import Data.Char

data AlexPosn = AlexPn !Int !Int !Int
	deriving (Eq,Show)

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


data Lexeme = L { tokenId :: Int,
                  startpos ::AlexPosn,
                  len :: Int,
                  lexClass :: LexemeClass,
                  str:: String }
              deriving Show

data LexemeClass
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
  deriving (Show,Eq,Ord,Enum,Ix)


showPosn (AlexPn _ line col) = show line ++ ':': show col


showToken (L id (AlexPn o l c) len LCspId str) = "built-in '"++str++"'"
showToken (L id (AlexPn o l c) len LCspBI str) = "built-in '"++str++"'"
showToken (L id (AlexPn o l c) len tokClass str) = "'"++str++"'"

tokenSentinel= 
 L { tokenId = -1
   , startpos = AlexPn 0 0 0
   , len =0
   ,lexClass =error "CSPLexer.x illegal access tokenSentinel"
   ,str=error "CSPLexer.x illegal access tokenSentinel"
   }
