module Language.CSPM.AlexWrapper
where

import Language.CSPM.Token

import Data.Ix
import Data.Typeable
import Data.Char

type AlexInput = (AlexPosn, 	-- current position,
		  Char,		-- previous char
		  String)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
				Just (c, (p', c, s))
alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

data AlexState = AlexState {
	alex_pos :: !AlexPosn,	-- position at current input location
	alex_inp :: String,	-- the current input
	alex_chr :: !Char,	-- the character before the input
	alex_scd :: !Int, 	-- the current startcode
	alex_cnt :: !Int 	-- nuber of tokens
    }

-- Compile with -funbox-strict-fields for best results!

runAlex :: String -> Alex a -> Either LexError a
runAlex input (Alex f) 
   = case f (AlexState {alex_pos = alexStartPos,
 			alex_inp = input,	
			alex_chr = '\n',
			alex_scd = 0,
                        alex_cnt = 0}) of Left msg -> Left msg
					  Right ( _, a ) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either LexError (AlexState, a) }

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of 
				Left msg -> Left msg
				Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_inp=inp} -> 
	Right (s, (pos,c,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp)
 = Alex $ \s -> case s{alex_pos=pos,alex_chr=c,alex_inp=inp} of
		  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ update where
    update st = Left $ LexError {lexEPos = alex_pos st, lexEMsg = message }
--    update st = Left ( message ++ " " ++( show (alex_pos st) ))
--    update (Right (st,_)) = Left ( message ++ ( show (alex_pos st) ))
--    update _ = Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())

alexNextToken :: Alex (Int)
alexNextToken = Alex $ \s@AlexState{alex_cnt=cnt} -> 
	Right (s {alex_cnt=(cnt+1)}, cnt)


-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> result

-- perform an action for this token, and set the start code to a new value
-- andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

-- token :: (String -> Int -> token) -> AlexAction token
token t input len = return (t input len)

mkL :: LexemeClass -> AlexInput -> Int -> Alex Lexeme
mkL c (pos,_,str) len = do
  cnt<-alexNextToken
  return (L cnt pos len c (take len str))

block_comment :: AlexInput -> Int -> Alex Lexeme
block_comment (startPos,_,_) _ = do
  inputs <- alexGetInput
  go 1 0 inputs inputs
  where go 0 count input (spos,_,str) = do
                  alexSetInput input
                  cnt<-alexNextToken
                  return (L cnt spos count LBComment (take (count-2) str) )
	go n count input i= do
	  case alexGetChar input of
	    Nothing  -> err input
	    Just (c,input) -> do
	      case c of
	    	'-' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('\125',input) -> go (n-1) (count+2) input i
		    Just (c,input)      -> go n (count+2) input i
	     	'\123' -> do
		  case alexGetChar input of
		    Nothing  -> err input
		    Just ('-',input) -> go (n+1) (count+2) input i
		    Just (c,input)   -> go n (count+2) input i
	    	c -> go n (count+1) input i

        err input = do alexSetInput input;
                       lexError $ "Unclosed Blockcomment  (starting at :"
                                   ++ (pprintAlexPosn startPos) ++") "

lexError s = do
  (p,c,input) <- alexGetInput
  alexError ( s ++ (if (not (null input))
		     then " at " ++ showChar (head input)
		     else " at end of file"))
  where
    showChar c = 
     if isPrint c then show c
       else "charcode " ++ (show $ ord c)


alexEOF :: Alex Lexeme
alexEOF = return (L 0 (AlexPn 0 0 0) 0 LEOF "")
