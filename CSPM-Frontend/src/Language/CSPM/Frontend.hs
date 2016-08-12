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
  ,parseWithoutSrcLoc
  ,ParseError(..)

  -- Language.CSPM.Utils
  ,parseFile
  ,parseString
  ,parseNamedString
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
  ,renameModuleReset
  ,RenameInfo (..)
  ,RenameError (..)

-- AstUtils
  ,removeSourceLocations
  ,removeParens
  ,unUniqueIdent
  ,computeFreeNames
  ,setNodeIdsZero
  ,getLastBindExpression
  ,getLastDeclaration
-- Versions
  ,frontendVersion
)
where

import Language.CSPM.Parser (ParseError(..), parse, parseWithoutSrcLoc)
import Language.CSPM.Rename
  (RenameError (..), RenameInfo (..), renameModule, renameModuleReset, ModuleFromRenaming)
import Language.CSPM.Token (Token, LexError(..))
import Language.CSPM.AST
  (Labeled (..), Module (..), Bindings, castModule, ModuleFromParser)
import Language.CSPM.SrcLoc (SrcLoc(..))
import Language.CSPM.AstUtils 
  (removeSourceLocations,removeParens ,unUniqueIdent
 , computeFreeNames, setNodeIdsZero, getLastBindExpression, getLastDeclaration)
import qualified Language.CSPM.LexHelper as Lexer
  (lexInclude, lexPlain, removeIgnoredToken)
import Language.CSPM.PrettyPrinter ()
import Language.CSPM.Utils
  (eitherToExc, handleLexError, handleParseError, handleRenameError
  ,parseFile, parseString, parseNamedString, benchmarkFrontend)
import Paths_CSPM_Frontend (version)
import Data.Version (Version)

-- | The version of the CSPM-Frontend library
frontendVersion :: Version
frontendVersion = version
