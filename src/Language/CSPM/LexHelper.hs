module Language.CSPM.LexHelper
(
   lexInclude
  ,lexPlain
  ,Lexer.Lexeme(..), Lexer.LexemeClass(..), Lexer.LexError(..) -- reexports
  ,Lexer.alexLine, Lexer.alexCol, Lexer.alexPos
  ,filterIgnoredToken
  ,tokenIsIgnored
  ,tokenIsComment
  ,tokenIsFDR
)
where

import Language.CSPM.Lexer as Lexer
  (Lexeme(..), LexemeClass(..), LexError(..), scanner
  ,alexLine, alexCol, alexPos )

{- todo : use an error monad -}

lexInclude :: String -> IO (Either LexError [Lexeme])
lexInclude src = do
  case scanner src of
    Left err -> return $ Left err
    Right toks -> do
      tokenIncl <- processIncludeAndReverse toks
      case tokenIncl of
        Left err -> return $ Left err
        Right t -> return $ Right t

lexPlain :: String -> Either LexError [Lexeme]
lexPlain src = fmap reverse $ scanner src

processIncludeAndReverse :: [Lexeme] -> IO (Either LexError [Lexeme] )
processIncludeAndReverse tokens = picl_acc tokens []
  where 
  picl_acc ::[Lexeme] ->[Lexeme] -> IO (Either LexError [Lexeme] )
  picl_acc [] acc = return $ Right acc
  picl_acc ((L _ _ _ LString fname) : (L _ _ _ LInclude _) :trest) acc = do
    let fileName = reverse $ tail $ reverse $ tail fname -- remove quotes
    -- putStrLn $ "Including file : " ++ fileName
    input <-readFile fileName
    case scanner input of
      Right toks -> do
        new_acc <- picl_acc toks acc
        case new_acc of
          Right t -> picl_acc trest t
          e -> return e
      Left e -> return $ Left e
  picl_acc ((incl@(L _ _ _ LInclude _)) : _) _ = 
    return $ Left $ LexError {
       lexEPos = startpos incl
      ,lexEMsg = "Include without filename" 
      }
  picl_acc (h:rest) acc = picl_acc rest $ h:acc


filterIgnoredToken :: [Lexeme] -> [Lexeme]
filterIgnoredToken = filter ( not . tokenIsIgnored)

tokenIsIgnored :: Lexeme -> Bool
tokenIsIgnored (Lexer.L _ _ _ LLComment _) = True
tokenIsIgnored (Lexer.L _ _ _ LCSPFDR _) = True
tokenIsIgnored (Lexer.L _ _ _ LBComment _) = True
tokenIsIgnored _ = False

tokenIsComment :: Lexeme -> Bool
tokenIsComment (Lexer.L _ _ _ LLComment _) = True
tokenIsComment (Lexer.L _ _ _ LBComment _) = True
tokenIsComment _ = False

tokenIsFDR :: Lexeme -> Bool
tokenIsFDR (Lexer.L _ _ _ LCSPFDR _) = True
tokenIsFDR _ = False
