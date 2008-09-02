{
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Language.CSPM.Lexer 
(
scanner
,Lexeme(..)
,LexemeClass(..),AlexPosn(..),LexError(..)
,allLexTypes
,alexLine,alexCol,alexPos
,pprintAlexPosn
,tokenSentinel
,alexMove
,showToken
)
where
import Data.Char
}

$whitechar = [\ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

--$large     = [A-Z \xc0-\xd6 \xd8-\xde]
--$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$large     = [A-Z]
$small     = [a-z]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit	   = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \' \_]
$symchar   = [$symbol \:]
$nl        = [\n\r]

@cspid = 
        channel|datatype|nametype|subtype
        | assert |pragma|transparent|external|print
        | STOP|SKIP|true|false|if|then|else|let|within
        | not | and | or | Int | Bool
        | Events

@cspbi =
  CHAOS
  | union | inter | diff | Union | Inter | member | card
  | empty | set | Set | Seq
  | null | head | tail | concat | elem | length

@cspsym = 
     "(" | ")" | "<" | ">" | "[" | "]" | "[[" | "]]" | "[|" | "|]"
     | "{" | "}" | "{|" | "|}"
     | "[>" | "[]" | "|~|" | "\" | "|" | "||" | "|||"
     | "->" | "<-" | "<->" 
     | ":" | "/\" | "&" | ";" | ".."
     | "." | "?" | "!" | "@" | "," | "=" | "@@"
     | "==" | ">=" | "<=" | "!="
     | "+" | "-" | "*" | "/" | "%" | "#" | "^"
     | "_"
     | "[FD=" | "[F=" | "[T="

@ident = $alpha $idchar*

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*

@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal

$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	 | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap

@assertExts = $whitechar+ "[FD]" | $whitechar+ "[F]"
@assertCore = "deterministic" @assertExts?
            | "livelock" $whitechar+ "free" @assertExts?
            | "deadlock" $whitechar+ "free" @assertExts?
            | "divergence" $whitechar+ "free" @assertExts?

csp :-

<0> $white+			{ skip }
<0> "--".*			{ mkL LLComment }
"{-"				{ block_comment }
--<0> "include"\-*[^$symbol].*    { mkL LCSPFDR }


-- Fixme : tread this properly
<0> ":[" $whitechar* @assertCore $whitechar* "]"    { mkL LCSPFDR }

<0> "include"                   { mkL LInclude }

<0> @cspid			{ mkL LCspId }

<0> @cspbi			{ mkL LCspBI }

<0> @cspsym                     { mkL LCspsym} -- ambiguity for wildcardpattern _

<0> @ident                      { mkL LIdent}  -- ambiguity for wildcardpattern _

<0> @decimal 
  | 0[oO] @octal
  | 0[xX] @hexadecimal		{ mkL LInteger }


-- <0> \' ($graphic # [\'\\] | " " | @escape) \' { mkL LChar }

   <0> \" @string* \"		{ mkL LString }

{
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn, 	-- current position,
		  Char,		-- previous char
		  String)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,s) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,c,[]) = Nothing
alexGetChar (p,_,(c:s))  = let p' = alexMove p c in p' `seq`
				Just (c, (p', c, s))

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
	deriving (Eq,Show)

alexLine (AlexPn _ l _) =l
alexCol (AlexPn _ _ c) =c
alexPos (AlexPn p _ _) =p

pprintAlexPosn (AlexPn p l c) = "Line: "++show l++" Col: "++show c

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)
{-
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     ((c `div` 8)*8+8)
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
-}

-- -----------------------------------------------------------------------------
-- Default monad


data AlexState = AlexState {
	alex_pos :: !AlexPosn,	-- position at current input location
	alex_inp :: String,	-- the current input
	alex_chr :: !Char,	-- the character before the input
	alex_scd :: !Int, 	-- the current startcode
	alex_cnt :: !Int 	-- nuber of tokens
    }

-- Compile with -funbox-strict-fields for best results!

data LexError = LexError {
   lexEPos :: !AlexPosn
  ,lexEMsg :: !String
  } deriving Show

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

alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (pos,chr,h:rest)
         -> lexError $ "lexical error"
    AlexSkip  inp' len -> do
	alexSetInput inp'
	alexMonadScan
    AlexToken inp' len action -> do
	alexSetInput inp'
	action inp len

-- -----------------------------------------------------------------------------
-- Useful token actions

type AlexAction result = AlexInput -> Int -> result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
-- andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

-- token :: (String -> Int -> token) -> AlexAction token
token t input len = return (t input len)


id = tokenId
data Lexeme = L { tokenId :: Int,
                  startpos ::AlexPosn,
                  len :: Int,
                  lexClass :: LexemeClass,
                  str:: String }
              deriving Show

data LexemeClass
  = LInteger
--  | LFloat
--  | LChar
  | LString
  | LCspId
  | LCspBI
  | LCspsym
  | LIdent
  | LCSPFDR
  | LLComment
  | LBComment
  | LEOF
  | LInclude
  deriving (Show,Eq,Ord,Enum,Ix)

allLexTypes=[ LInteger,LCspId,LCspBI,LCspsym,LIdent
 ,LCSPFDR,LLComment,LBComment,LEOF ]
  
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

tokenSentinel= 
 L { tokenId = -1
   , startpos = AlexPn 0 0 0
   , len =0
   ,lexClass =error "CSPLexer.x illegal access tokenSentinel"
   ,str=error "CSPLexer.x illegal access tokenSentinel"
   }
{--
scanner str = runAlex str $ do
  let loop i = do tok@(L _ cl _) <- alexMonadScan; 
		  if cl == LEOF
			then return i
			else do loop $! (i+1)
  loop 0
--}


scanner str = runAlex str $ do
  let loop i = do tok@(L _ _ _ cl _) <- alexMonadScan; 
		  if cl == LEOF
			then return i
			else do loop $! (tok:i)
  loop []

--alexEOF = return (L undefined LEOF "")
alexEOF = return (L 0 (AlexPn 0 0 0) 0 LEOF "")

showPosn (AlexPn _ line col) = show line ++ ':': show col

--main = do
--  s <- getContents
--  print (scanner s)
--  print (scannerb s)

tokIsIgnored (L _ _ _ LLComment _) = True
tokIsIgnored (L _ _ _ LCSPFDR _) = True
tokIsIgnored (L _ _ _ LBComment _) = True
tokIsIgnored _ = False

tokIsComment (L _ _ _ LLComment _) = True
tokIsComment (L _ _ _ LBComment _) = True
tokIsComment _ = False

tokIsFDR (L _ _ _ LCSPFDR _) = True
tokIsFDR _ = False

showToken (L id (AlexPn o l c) len LCspId str) = "built-in '"++str++"'"
showToken (L id (AlexPn o l c) len LCspBI str) = "built-in '"++str++"'"
showToken (L id (AlexPn o l c) len tokClass str) = "'"++str++"'"

}
