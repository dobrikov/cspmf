{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Language.CSPM.TestScript
where

import Language.CSPM.AST
import Language.CSPM.Utils(parseFile, parseString)
import Language.CSPM.PrettyPrinter
import qualified Language.CSPM.LexHelper as Lexer (lexInclude)
import Language.CSPM.AstUtils(removeParens, removeSourceLocations, unUniqueIdent)
import Language.CSPM.SrcLoc
import Language.CSPM.Parser(parse)
import System.FilePath(splitExtension)

testPrettyParser :: FilePath -> IO Bool
testPrettyParser f = 
  do 
   ast1 <- parseFile f
   let (fileName,ext) = splitExtension f
   writeFile (fileName ++ "Pretty.csp") (show $ pPrint ast1)
   ast2 <- parseString (show $ pPrint  ast1) 
   writeFile (fileName ++ "1.ast") (show $ labeled ast1)
   writeFile (fileName ++ "2.ast") (show $ labeled ast2)
   return $ (==) (labeled ast1) (labeled ast2) -- instances of the class Eq needed to be implemented here for correct AST equality check

