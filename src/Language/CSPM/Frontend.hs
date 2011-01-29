-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Frontend
-- Copyright   :  (c) Fontaine 2008
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
  ,getRenaming
  ,applyRenaming
  -- AstUtils
  ,removeSourceLocations
  ,removeParens
  ,removeModuleTokens
  ,unUniqueIdent
  ,showAst
  ,relabelAst
  ,computeFreeNames
  --
  ,RenameError(..)
  ,eitherToExc
  ,handleLexError
  ,handleParseError
  ,handleRenameError
  ,compilePattern
  ,version
  ,pp
  ,compareAST
)
where

import Language.CSPM.Parser (ParseError(..),parse)
import Language.CSPM.Rename (RenameError(..),getRenaming,applyRenaming)
import Language.CSPM.PatternCompiler (compilePattern)
import Language.CSPM.Token (Token,LexError(..))
import Language.CSPM.AST (Labeled(..),LModule,Module(..),Bindings)
import Language.CSPM.SrcLoc (SrcLoc(..))
import Language.CSPM.AstUtils 
  (removeSourceLocations,removeModuleTokens,removeParens,relabelAst
  ,unUniqueIdent,showAst,computeFreeNames, compareAST)

import qualified Language.CSPM.LexHelper as Lexer
  (lexInclude,lexPlain,filterIgnoredToken)
import Language.CSPM.Version

import Language.CSPM.PrettyPrinter (pp)
import Language.CSPM.Utils
  (eitherToExc,handleLexError,handleParseError,handleRenameError
  ,parseFile,testFrontend)
