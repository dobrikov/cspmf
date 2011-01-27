module Language.CSPM.LexHelper
(
   lexInclude
  ,lexPlain
  ,filterIgnoredToken
  ,tokenIsIgnored
  ,tokenIsComment
)
where

import qualified Language.CSPM.Lexer as Lexer (scanner)
import Language.CSPM.Token (Token(..), LexError(..) )
import Language.CSPM.TokenClasses (PrimToken(..))
--import qualified Language.CSPM.Token as Token
--  (Token(..), LexError(..))

{- todo : use an error monad -}

lexInclude :: String -> IO (Either LexError [Token])
lexInclude src = do
  case Lexer.scanner src of
    Left err -> return $ Left err
    Right toks -> do
      tokenIncl <- processIncludeAndReverse toks
      case tokenIncl of
        Left err -> return $ Left err
        Right t -> return $ Right t

lexPlain :: String -> Either LexError [Token]
lexPlain src = fmap reverse $ Lexer.scanner src

processIncludeAndReverse :: [Token] -> IO (Either LexError [Token] )
processIncludeAndReverse tokens = picl_acc tokens []
  where 
  picl_acc ::[Token] ->[Token] -> IO (Either LexError [Token] )
  picl_acc [] acc = return $ Right acc
  picl_acc ((Token _ _ _ L_String fname) : (Token _ _ _ L_Include _) :trest) acc = do
    let fileName = reverse $ tail $ reverse $ tail fname -- remove quotes
    -- putStrLn $ "Including file : " ++ fileName
    input <-readFile fileName
    case Lexer.scanner input of
      Right toks -> do
        new_acc <- picl_acc toks acc
        case new_acc of
          Right t -> picl_acc trest t
          e -> return e
      Left e -> return $ Left e
  picl_acc ((incl@(Token _ _ _ L_Include _)) : _) _ = 
    return $ Left $ LexError {
       lexEPos = tokenStart incl
      ,lexEMsg = "Include without filename" 
      }
  picl_acc (h:rest) acc = picl_acc rest $ h:acc


filterIgnoredToken :: [Token] -> [Token]
filterIgnoredToken = filter ( not . tokenIsIgnored)

tokenIsIgnored :: Token -> Bool
tokenIsIgnored (Token _ _ _ L_LComment _) = True
tokenIsIgnored (Token _ _ _ L_BComment _) = True
tokenIsIgnored _ = False

tokenIsComment :: Token -> Bool
tokenIsComment (Token _ _ _ L_LComment _) = True
tokenIsComment (Token _ _ _ L_BComment _) = True
tokenIsComment _ = False
