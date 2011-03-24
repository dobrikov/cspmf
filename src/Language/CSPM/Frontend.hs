-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Frontend
-- Copyright   :  (c) Fontaine 2008 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Frontend contains some reexports from other modules

module Language.CSPM.Frontend
(
  -- Language.CSPM.Parser
   parse
  ,ParseError(..)

  -- Language.CSPM.Utils
  ,parseFile
  ,benchmarkFrontend
  ,eitherToExc
  ,handleLexError
  ,handleParseError
  ,handleRenameError

-- Language.CSPM.Token
  ,Token
  ,LexError(..)

-- Language.CSPM.LexHelper
  ,Lexer.lexInclude
  ,Lexer.lexPlain
  ,Lexer.removeIgnoredToken

-- Language.CSPM.AST
  ,Module
  ,ModuleFromParser
  ,ModuleFromRenaming
  ,Labeled (..)
  ,castModule

  ,Bindings
  ,SrcLoc (..)

-- Language.CSPM.Rename
  ,renameModule
  ,RenameInfo (..)
  ,getRenaming
  ,applyRenaming
  ,RenameError (..)

-- AstUtils
  ,removeSourceLocations
  ,removeParens
  ,removeModuleTokens
  ,unUniqueIdent
  ,showAst
  ,computeFreeNames
  ,setNodeIdsZero

-- PrettyPrinter
  ,pp
  ,prettyPrintFile
)
where

import Language.CSPM.Parser (ParseError(..), parse)
import Language.CSPM.Rename
  (RenameError (..), RenameInfo (..), renameModule, getRenaming, applyRenaming
  ,ModuleFromRenaming)
import Language.CSPM.Token (Token, LexError(..))
import Language.CSPM.AST
  (Labeled (..), Module (..), Bindings, castModule, ModuleFromParser)
import Language.CSPM.SrcLoc (SrcLoc(..))
import Language.CSPM.AstUtils 
  (removeSourceLocations, removeModuleTokens, removeParens
  ,unUniqueIdent, showAst, computeFreeNames, setNodeIdsZero)
import qualified Language.CSPM.LexHelper as Lexer
  (lexInclude, lexPlain, removeIgnoredToken)
import Language.CSPM.PrettyPrinter (pp, prettyPrintFile)
import Language.CSPM.Utils
  (eitherToExc, handleLexError, handleParseError, handleRenameError
  ,parseFile, benchmarkFrontend)
