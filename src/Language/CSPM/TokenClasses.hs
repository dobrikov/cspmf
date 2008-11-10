-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.TokenClasses
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  provisional
-- Portability :  GHC-only
--
-- This module contains the datatype Tokens.

{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Language.CSPM.TokenClasses
where

import Data.Typeable (Typeable)
import Data.Generics.Basics (Data)
import Data.Generics.Instances ()

data TokenClass
  = LInteger
  | LString
  | LCspKeyword Keyword
  | LCspBuiltIn BuiltIn     -- builtIns called by name
  | LCspId
  | LCspsym
  | LIdent
  | LCSPFDR      -- needed for special assertions
  | LLComment
  | LBComment
  | LEOF
  | LInclude
--  deriving (Show,Eq,Ord,Enum,Ix, Typeable, Data)
  deriving (Show,Eq,Ord, Typeable, Data)

--allTokenClasses :: [TokenClass]
--allTokenClasses = [LInteger .. LInclude]

-- csp-specific
data Keyword
  = T_channel
  | T_datatype
  | T_nametype
  | T_subtype
  | T_assert
  | T_pragma
  | T_transparent
  | T_external
  | T_print
  | T_if
  | T_then
  | T_else
  | T_let
  | T_within
  deriving (Show,Eq,Ord,Typeable, Data)

{-
builtin functions
-}
data BuiltIn
  = T_true
  | T_false
  | T_not
  | T_and
  | T_or
  | T_union
  | T_inter
  | T_diff
  | T_Union
  | T_Inter
  | T_member
  | T_card
  | T_empty
  | T_set
  | T_Set
  | T_Seq
  | T_null
  | T_head
  | T_tail
  | T_concat
  | T_elem
  | T_length
  | T_STOP
  | T_SKIP
  | T_Events
  | T_Int
  | T_Bool
  | T_CHAOS
{-
maybe it would be better to strictly distinct between token and semantics
todo : next time
-}
  | T_Concat       -- "^"
  | T_Len2         -- "#"
  | T_Mult         -- "*"
  | T_Div          -- "/"
  | T_Mod          -- "%"
  | T_Add          -- "+"
  | T_Sub          -- "-"
  | T_Eq           -- "=="
  | T_NEq          -- "!="
  | T_GE           -- ">="
  | T_LE           -- "<-"
  | T_LT           -- "<"
  | T_GT           -- ">"
  | T_Guard        -- "&"
  | T_Semicolon    -- ";"
  | T_Interrupt    -- "/\\"
  | T_ExtChoice    -- "[]"
  | T_Timeout      -- "[>"
  | T_IntChoice    -- "|~|"
  | T_Interleave   -- "|||"
  | T_Hiding       -- "\\"
  deriving (Show,Eq,Ord,Typeable, Data)