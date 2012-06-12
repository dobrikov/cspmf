----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.AlexWrapper
-- 
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Wrapper functions for Alex

{-# LANGUAGE RecordWildCards #-}
module Language.CSPM.AlexWrapper
where

import Language.CSPM.Token
import Language.CSPM.TokenClasses
import Language.CSPM.UnicodeSymbols as UnicodeSymbols (lookupToken)

import Data.Char
import Data.Word (Word8)
import qualified Data.Bits
import Data.List
import Control.Monad

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

type Byte = Word8

data AlexState = AlexState {
   alex_input :: AlexInput
  ,alex_scd :: !Int 	-- the current startcode
  ,alex_cnt :: !Int 	-- number of tokens
  }

runAlex :: String -> Alex a -> Either LexError a
runAlex input (Alex f) 
  = case f initState of
     Left msg -> Left msg
     Right ( _, a ) -> Right a
  where
    initState
      = AlexState {
       alex_input = initAlexInput
      ,alex_scd = 0
      ,alex_cnt = 0
      }
    initAlexInput = (alexStartPos,'\n',[],input)

newtype Alex a = Alex { unAlex :: AlexState -> Either LexError (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of
                            Left msg -> Left msg
                            Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
  = Alex $ \s-> Right (s,alex_input s)

alexSetInput :: AlexInput -> Alex ()
alexSetInput input
   = Alex $ \state -> case state {alex_input=input} of
            s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message
    = Alex $ \st -> let (pos,_,_,_) = alex_input st in
                 Left $ LexError {lexEPos = pos, lexEMsg = message }

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

-- increase token counter and return tokenCount
alexCountToken :: Alex Int
alexCountToken
  = Alex $ \s -> Right (s {alex_cnt = succ $ alex_cnt s}, alex_cnt s)

-- taken from original Alex-Wrapper
alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c 
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]


-- Useful token actions
type AlexAction result = AlexInput -> Int -> result

-- perform an action for this token, and set the start code to a new value
andBegin :: (t -> t1 -> Alex b) -> Int -> t -> t1 -> Alex b
andBegin action code input len = do alexSetStartCode code; action input len

mkL :: PrimToken -> AlexInput -> Int -> Alex Token
mkL c (pos, _, _, str) len = do
  cnt <- alexCountToken
  return $ Token {
    tokenId     = mkTokenId cnt
  , tokenStart  = pos
  , tokenLen    = len
  , tokenClass  = c
  , tokenString = take len str
  }

block_comment :: AlexInput -> Int -> Alex Token
block_comment (startPos, _ ,[], '\123':'-':input) 2 = do
    case go 1 "-{" input of
      Nothing -> Alex $ \_-> Left $ LexError {
         lexEPos = startPos
        ,lexEMsg = "Unclosed Blockcomment"
        }
      Just (acc, rest) -> do
        cnt <- alexCountToken
        let
          tokenId = mkTokenId cnt
          tokenString = reverse acc
          tokenLen = length tokenString
          tokenStart = startPos
          tokenClass = case (tokenString, acc) of
               ('\123':'-':'#':_, '\125':'-':'#':_) ->  L_Pragma
               ('\123':'-':_    , '\125':'-':_    ) ->  L_BComment
               _ -> error "internal Error: cannot determine variant of block_comment"
        alexSetInput (foldl' alexMove startPos tokenString, '\125', [],rest)
        return $ Token {..}
  where
    go :: Int -> String -> String -> Maybe (String,String)
    go 0 acc rest = Just (acc, rest)
    go nested acc rest = case rest of
      '-' : '\125' : r2 -> go (pred nested) ('\125': '-' : acc) r2
      '\123' : '-'  : r2 -> go (succ nested) ('-'  : '\123': acc) r2
      h:r2 -> go nested (h : acc) r2
      [] -> Nothing

block_comment _ _ = error "internal Error : block_comment called with bad args"

lexError :: String -> Alex a
lexError s = do
  (_p, _c,_, input) <- alexGetInput
  let
    pos = if not $ null input
            then " at " ++ (reportChar $ head input)
            else " at end of file"
  alexError $ s ++ pos
  where
    reportChar c =
     if isPrint c
       then show c
       else "charcode " ++ (show $ ord c)

alexEOF :: Alex Token
alexEOF = return (Token (mkTokenId 0) (AlexPn 0 0 0) 0 L_EOF "")

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p, _c, _,_s) = error "alex-input-prev-char not supported ??!"
