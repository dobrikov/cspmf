{-# LANGUAGE TypeSynonymInstances, FlexibleInstances#-}

module Language.CSPM.TestScript
where

import Language.CSPM.AST
import Language.CSPM.Utils(parseFile)
import Language.CSPM.PrettyPrinter(dropCsp, toPrettyString, runPretty)
import Language.CSPM.AstUtils(removeParens, removeModuleTokens, removeSourceLocations, unUniqueIdent, compareAST)
import Language.CSPM.SrcLoc

simplifyAst :: LModule -> LModule
simplifyAst ast = removeParens $ removeSourceLocations $ removeModuleTokens ast

testPrettyParser :: FilePath -> IO Bool
testPrettyParser f = 
  do 
   str1 <- parseFile f
   let fileName = dropCsp f
   writeFile (fileName ++ "Pretty.csp") (toPrettyString str1)
   str2 <- parseFile (fileName ++ "Pretty.csp") 
   return $ compareAST (simplifyAst str1) (simplifyAst str2)

