{-# LANGUAGE TupleSections, RecordWildCards #-}
module Language.CSPM.LexHelper
{-
(
   lexInclude
  ,lexPlain
  ,removeIgnoredToken
  ,tokenIsComment
  ,unicodeTokenString
  ,asciiTokenString
)
-}
where

import qualified Language.CSPM.Lexer as Lexer (scanner)
import Language.CSPM.Token (Token(..), LexError(..))
import Language.CSPM.TokenClasses (PrimToken(..))
import Language.CSPM.UnicodeSymbols (lookupDefaultSymbol)
import Control.Monad.IO.Class
import qualified Data.Set as Set

import qualified Data.DList as DList
import Control.Monad.Trans.Either

-- | lex a String .
lexPlain :: String -> Either LexError [Token]
lexPlain src = fmap reverse $ Lexer.scanner src

-- | Convert a token to a String.
--   If the tokenClasss has a Unicode symbol return the default Unicode string.
unicodeTokenString :: Token -> String
unicodeTokenString token
  = case lookupDefaultSymbol $ tokenClass token of
      Just (unicodeSymbol, _) -> [unicodeSymbol]
      Nothing -> tokenString token

-- | Convert a token to a String.
--   If the tokenClasss has a Unicode symbol return the default ASCII string.
asciiTokenString :: Token -> String
asciiTokenString token
  = case lookupDefaultSymbol $ tokenClass token of
      Just (_, symbol) -> symbol
      Nothing -> tokenString token

type Chunk = [Token]
type Chunks = DList.DList Chunk
data FilePart
    = Toks    Chunk
    | Include FilePath
    deriving Show

-- | lex input-string and inport all includes files
lexInclude :: String -> IO (Either LexError [Token])
lexInclude input
   = eitherT (return . Left) (return . Right . concat . DList.toList) $ lexInclude2 input

lexInclude2 :: String -> EitherT LexError IO Chunks
lexInclude2 input = do
        hoistEither $ lexPlain input
    >>= hoistEither . splitIncludes []
    >>= mapM processPart
    >>= return . DList.concat

processPart :: FilePart -> EitherT LexError IO Chunks
processPart part = case part of
    Toks ch -> return $ DList.singleton $ ch
    Include fname -> (liftIO $ readFile fname) >>= lexInclude2

-- | micro-parser for include-statements
splitIncludes :: [Token] -> [Token] -> Either LexError [FilePart]
splitIncludes acc  []  = return [Toks $ reverse acc]
splitIncludes acc (h:rest) = case h of
    tok@(Token _ _ _ L_Include _) -> do
        r <- scanInclude tok rest
        return $ (Toks $ reverse acc) : r
    _ -> splitIncludes (h:acc) rest

scanInclude :: Token -> [Token] -> Either LexError [FilePart]
scanInclude incl (h:rest) = case h of
    Token _ _ _ T_WhiteSpace _ -> scanInclude incl rest
    Token _ _ _ L_String fname -> do
       r <- splitIncludes [] rest
       let fileName = reverse $ tail $ reverse $ tail fname -- remove quotes
       return $ (Include fileName) : r
    _ -> Left $ LexError {
       lexEPos = tokenStart incl
      ,lexEMsg = "Include without filename"
      }


scanInclude incl _ = Left $ LexError {
       lexEPos = tokenStart incl
      ,lexEMsg = "Include without filename at end of file"
      }


-- | Remove comments, whitespaces and unneeded newlines.
removeIgnoredToken :: [Token] -> [Token]
removeIgnoredToken = soakNewlines . removeComments
  where
    -- | Remove comments from the token stream.
    removeComments :: [Token] -> [Token]
    removeComments = filter (\t -> not (tokenIsComment t || isWhiteSpace t))
    isWhiteSpace = (==) T_WhiteSpace . tokenClass

-- | Is the token a line-comment, block-comment or a Pragma?
tokenIsComment :: Token -> Bool
tokenIsComment t = tc == L_LComment || tc == L_BComment || tc == L_Pragma
  where tc = tokenClass t


-- | remove newlines, that do not end a declaration from the token stream.
-- For example newlines next to binary operators.
-- Remove all trailing newlines.
soakNewlines :: [Token] -> [Token]
soakNewlines = worker
  where
    worker [] = []
    worker [x] | tokenClass x ==L_Newline = []
    worker [x] = [x]
    worker (h1:h2:t) = case (tokenClass h1, tokenClass h2) of
       (L_Newline, L_Newline) -> worker (h1:t)
       (L_Newline, _) | isH2NewLineConsumer -> worker $ h2:t
       (L_Newline, _) -> h1 : (worker $ h2:t)
       (_, L_Newline) | isH1NewLineConsumer -> worker $ h1:t
       (_, L_Newline) -> h1: (worker $ h2:t)
       _   -> h1: (worker $ h2:t)
      where
        isH2NewLineConsumer = tokenClass h2 `Set.member` consumeNLBeforeToken
        isH1NewLineConsumer = tokenClass h1 `Set.member` consumeNLAfterToken

    binaryOperators =
     [T_is, T_hat, T_hash, T_times, T_slash,
      T_percent, T_plus, T_minus, T_eq, T_neq,
      T_ge, T_le, T_not, T_amp, T_semicolon,
      T_comma, T_triangle, T_box, T_rhd, T_exp,
      T_sqcap, T_interleave, T_backslash, T_parallel,
      T_mid, T_at, T_atat, T_rightarrow, T_leftarrow,
      T_leftrightarrow, T_dot, T_dotdot, T_exclamation,
      T_questionmark, T_colon, T_openBrack, T_closeBrack,
      T_openOxBrack, T_closeOxBrack,T_openPBrace,
      T_openBrackBrack, T_if, T_then,T_else, T_let, T_and,
      T_or, T_Refine, T_trace,T_failure, T_failureDivergence,
      T_refusalTesting, T_refusalTestingDiv, T_revivalTesting,
      T_revivalTestingDiv,T_tauPriorityOp, T_within]
    consumeNLBeforeToken
      = Set.fromList (
            [T_closeParen, T_gt, T_closeBrace, T_closeBrackBrack, T_closeSpecialBrack, T_closePBrace]
         ++ binaryOperators)
    consumeNLAfterToken
      = Set.fromList ( [T_openParen, T_openBrace, T_lt] ++ binaryOperators)
