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

data PrimToken
  = L_Integer
  | L_String
  | L_Ident
  | L_CSPFDR      -- needed for special assertions
  | L_LComment
  | L_BComment
  | L_EOF
  | L_Include
  | T_Refine
-- keywords
  | T_channel
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
-- constants and builtins
  | T_true
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
-- symbols
  | T_hat          -- "^"
  | T_hash         -- "#"
  | T_times        -- "*"
  | T_slash        -- "/"
  | T_percent      -- "%"
  | T_plus         -- "+"
  | T_minus        -- "-"
  | T_eq           -- "=="
  | T_neq          -- "!="
  | T_ge           -- ">="
  | T_le           -- "<="
  | T_lt           -- "<"
  | T_gt           -- ">"
  | T_amp          -- "&"
  | T_semicolon    -- ";"
  | T_comma        -- ","
  | T_triangle     -- "/\\"
  | T_box          -- "[]"
  | T_rhd          -- "[>"
  | T_sqcap        -- "|~|"
  | T_interleave   -- "|||"
  | T_backslash    -- "\\"
  | T_parallel     -- "||"
  | T_mid          -- "|"
  | T_at           -- "@"
  | T_atat         -- "@@"
  | T_rightarrow   -- "->"
  | T_leftarrow    -- "<-"
  | T_leftrightarrow -- "<->"
  | T_dot          -- "."
  | T_dotdot       -- ".."
  | T_exclamation  -- "!"
  | T_questionmark -- "?"
  | T_colon        -- ":"
  | T_openParen    -- "("
  | T_closeParen   -- ")"
  | T_openBrace    -- "{"
  | T_closeBrace   -- "}"
  | T_openBrack    -- "["
  | T_closeBrack   -- "]"
  | T_openOxBrack  -- "[|"
  | T_closeOxBrack -- "|]"
  | T_openBrackBrack  -- "[["
  | T_closeBrackBrack -- "]]"
  | T_openPBrace   -- "{|"
  | T_closePBrace  -- "|}"
  | T_underscore   -- "_"
  | T_is           -- "="
  deriving (Show,Eq,Ord,Typeable, Data)
