{-# LANGUAGE TypeSynonymInstances, FlexibleInstances#-}

module Language.CSPM.TestScript
where

import Language.CSPM.AST
import Language.CSPM.Utils(parseFile, parseString)
import Language.CSPM.PrettyPrinter(dropCsp, toPrettyString, runPretty)
import qualified Language.CSPM.LexHelper as Lexer (lexInclude)
import Language.CSPM.AstUtils(removeParens, removeModuleTokens, removeSourceLocations, unUniqueIdent, annulNodeId)
import Language.CSPM.SrcLoc
import Language.CSPM.Parser(parse)

--simplifyAst :: LModule -> LModule
simplifyAst ast = annulNodeId $ removeParens $ removeSourceLocations $ removeModuleTokens ast

testPrettyParser :: FilePath -> IO Bool
testPrettyParser f = 
  do 
   ast1 <- parseFile f
   let fileName = dropCsp f
   writeFile (fileName ++ "Pretty.csp") (toPrettyString ast1)
   ast2 <- parseString (toPrettyString  ast1) fileName 
   writeFile (fileName ++ "1.ast") (show $ simplifyAst $ labeled ast1)
   writeFile (fileName ++ "2.ast") (show $ simplifyAst $ labeled ast2)
   return $ (==) (simplifyAst $ labeled ast1) (simplifyAst $ labeled ast2)

