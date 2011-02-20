-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Frontend
-- Copyright   :  (c) Fontaine 2008 - 2011
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Frontend contains some reexports from other modules

module Language.CSPM.Frontend
(
   testFrontend
  ,parseFile
  ,Token
  ,Lexer.lexInclude
  ,Lexer.lexPlain
  ,Lexer.filterIgnoredToken
  ,LexError(..)
  ,parse
  ,ParseError(..)
  ,Module
  ,LModule
  ,Labeled(..)
  ,Bindings
  ,SrcLoc (..)
  ,renameModule
  ,RenameInfo (..)
  ,getRenaming
  ,applyRenaming
  ,castModule
  -- AstUtils
  ,removeSourceLocations
  ,removeParens
  ,removeModuleTokens
  ,unUniqueIdent
  ,showAst
  ,computeFreeNames
  --
  ,RenameError(..)
  ,eitherToExc
  ,handleLexError
  ,handleParseError
  ,handleRenameError
  ,pp
)
where

import Language.CSPM.Parser (ParseError(..),parse)
import Language.CSPM.Rename
  (RenameError (..), RenameInfo (..), renameModule, getRenaming, applyRenaming)
import Language.CSPM.Token (Token,LexError(..))
import Language.CSPM.AST
  (Labeled (..), LModule, Module (..), Bindings, castModule)
import Language.CSPM.SrcLoc (SrcLoc(..))
import Language.CSPM.AstUtils 
  (removeSourceLocations, removeModuleTokens, removeParens
  ,unUniqueIdent, showAst, computeFreeNames)

import qualified Language.CSPM.LexHelper as Lexer
  (lexInclude, lexPlain, filterIgnoredToken)

import Language.CSPM.PrettyPrinter (pp)
import Language.CSPM.Utils
  (eitherToExc, handleLexError, handleParseError, handleRenameError
  ,parseFile, testFrontend)
