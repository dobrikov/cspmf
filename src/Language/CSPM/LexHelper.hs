module Language.CSPM.LexHelper
(
   lexInclude
  ,lexPlain
)
where

import Language.CSPM.Lexer as Lexer (Lexeme(..),LexemeClass(..),LexError(..),scanner,startpos)

{- todo : use an error monad -}

lexInclude :: String -> IO (Either LexError [Lexeme])
lexInclude src = do
  case scanner src of
    Left err -> return $ Left err
    Right toks -> do
      tokenIncl <- processIncludeAndReverse toks
      case tokenIncl of
        Left err -> return $ Left err
        Right tokenIncl -> return $ Right tokenIncl

lexPlain :: String -> Either LexError [Lexeme]
lexPlain src = fmap reverse $ scanner src

processIncludeAndReverse :: [Lexeme] -> IO (Either LexError [Lexeme] )
processIncludeAndReverse tokens = picl_acc tokens []
  where 
  picl_acc ::[Lexeme] ->[Lexeme] -> IO (Either LexError [Lexeme] )
  picl_acc [] acc = return $ Right acc
  picl_acc ((L _ _ _ LString str) :(incl@(L _ _ _ LInclude _)) :trest) acc = do
    let fileName = reverse $ tail $ reverse $ tail str -- remove quotes
    input <-putStrLn $ "Including file : " ++ fileName
    input <-readFile fileName
    case scanner input of
      Right toks -> do
        new_acc <- picl_acc toks acc
        case new_acc of
          Right t -> picl_acc trest t
          e -> return e
      Left e -> return $ Left e
  picl_acc ((incl@(L _ _ _ LInclude str)):trest) acc = 
    return $ Left $ LexError {
       lexEPos = startpos incl
      ,lexEMsg = "Include without filename" 
      }
  picl_acc (h:rest) acc = picl_acc rest $ h:acc
