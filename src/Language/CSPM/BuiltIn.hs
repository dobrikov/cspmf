----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.BuiltIn
-- Copyright   :  (c) Fontaine 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The FDR supports some CSPM-built-ins that are NOT part of the CSPM-syntax.
-- 
-- Currently those built-ins are hard-coded in CSPM-Frontend.
-- We use Language.CSPM.BuiltIn as a compatibility layer to migrate to
-- FDR-style built-ins.
--
--
module Language.CSPM.BuiltIn
where

{-
FDR keywords are:
STOP, SKIP, true, false 
"assert"
"channel"
"datatype"
"subtype"
"freetype"
"nametype"
"pragma"{t}.*
"transparent"
"external"
"print"
"module"
"endmodule"
"exports"
"instance"
"include"{t}.*\n
maybe also: "and","or" and "not"

everything else in FDR is "built-in" but not syntax.
-}

-- | INCOMPLETE list of built-ins that are not CSPM-syntax.
-- | Todo: add more BuiltIns here are migrate the AST.
builtIns :: [String]
builtIns = [
   "Proc"
  ,"seq"
  ]