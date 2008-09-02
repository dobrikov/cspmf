module Language.CSPM.LexHelper
(
   runScanner
)
where

import Language.CSPM.Lexer as Lexer (Lexeme(..),LexemeClass(..),LexError(..),scanner,startpos)

{- todo : use an error monad -}

runScanner :: FilePath -> IO (Either LexError [Lexeme])
runScanner inFile = do
  input <- readFile inFile
  case scanner input of
    Left err -> return $ Left err
    Right toks -> do
      tokenIncl <- processIncludeAndReverse toks
      case tokenIncl of
        Left err -> return $ Left err
        Right tokenIncl -> return $ Right$ filter ( not . tokIsIgnored ) tokenIncl

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

tokIsIgnored :: Lexeme -> Bool
tokIsIgnored (L _ _ _ LLComment _) = True
tokIsIgnored (L _ _ _ LCSPFDR _) = True
tokIsIgnored (L _ _ _ LBComment _) = True
tokIsIgnored _ = False

tokIsComment :: Lexeme -> Bool
tokIsComment (L _ _ _ LLComment _) = True
tokIsComment (L _ _ _ LBComment _) = True
tokIsComment _ = False

tokIsFDR :: Lexeme -> Bool
tokIsFDR (L _ _ _ LCSPFDR _) = True
tokIsFDR _ = False
