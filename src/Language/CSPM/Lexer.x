{
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -cpp #-}

module Language.CSPM.Lexer 
(
scanner
)
where
import Language.CSPM.Token
import Language.CSPM.TokenClasses
import Language.CSPM.AlexWrapper
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

-- CSP-M Keywords
<0> "channel"     { mkKeyword T_channel  }
<0> "datatype"    { mkKeyword T_datatype }
<0> "nametype"    { mkKeyword T_nametype }
<0> "subtype"     { mkKeyword T_subtype }
<0> "assert"      { mkKeyword T_assert }
<0> "pragma"      { mkKeyword T_pragma }
<0> "transparent"    { mkKeyword T_transparent }
<0> "external"    { mkKeyword T_external }
<0> "print"    { mkKeyword T_print }
<0> "if"    { mkKeyword T_if }
<0> "then"    { mkKeyword T_then }
<0> "else"    { mkKeyword T_else }
<0> "let"    { mkKeyword T_let }
<0> "within"    { mkKeyword T_within }

-- CSP-M builtIns

<0> "STOP"    { mkBuiltIn T_STOP }
<0> "SKIP"    { mkBuiltIn T_SKIP }
<0> "true"    { mkBuiltIn T_true }
<0> "false"    { mkBuiltIn T_false }
<0> "not"    { mkBuiltIn T_not }
<0> "and"    { mkBuiltIn T_and }
<0> "or"    { mkBuiltIn T_or }
<0> "Int"    { mkBuiltIn T_Int }
<0> "Bool"    { mkBuiltIn T_Bool }
<0> "Events"    { mkBuiltIn T_Events }
<0> "CHAOS"    { mkBuiltIn T_CHAOS }
<0> "union"    { mkBuiltIn T_union }
<0> "inter"    { mkBuiltIn T_inter }
<0> "diff"    { mkBuiltIn T_diff }
<0> "Union"    { mkBuiltIn T_Union }
<0> "Inter"    { mkBuiltIn T_Inter }
<0> "member"    { mkBuiltIn T_member }
<0> "card"    { mkBuiltIn T_card }
<0> "empty"    { mkBuiltIn T_empty }
<0> "set"    { mkBuiltIn T_set }
<0> "Set"    { mkBuiltIn T_Set }
<0> "Seq"    { mkBuiltIn T_Seq }
<0> "null"    { mkBuiltIn T_null }
<0> "head"    { mkBuiltIn T_head }
<0> "tail"    { mkBuiltIn T_tail }
<0> "concat"    { mkBuiltIn T_concat }
<0> "elem"    { mkBuiltIn T_elem }
<0> "length"    { mkBuiltIn T_length }

<0> $white+			{ skip }
<0> "--".*			{ mkL LLComment }
"{-"				{ block_comment }

-- Fixme : tread this properly
<0> ":[" $whitechar* @assertCore $whitechar* "]"    { mkL LCSPFDR }

<0> "include"                   { mkL LInclude }

<0> @cspsym                     { mkL LCspsym} -- ambiguity for wildcardpattern _

<0> @ident                      { mkL LIdent}  -- ambiguity for wildcardpattern _

<0> @decimal 
  | 0[oO] @octal
  | 0[xX] @hexadecimal		{ mkL LInteger }


-- <0> \' ($graphic # [\'\\] | " " | @escape) \' { mkL LChar }

   <0> \" @string* \"		{ mkL LString }




{
alexMonadScan = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of                    -- alexScan is jump to alexGenerated code
    AlexEOF -> alexEOF
    AlexError (pos,chr,h:rest)
         -> lexError $ "lexical error"
    AlexSkip  inp' len -> do
	alexSetInput inp'
	alexMonadScan
    AlexToken inp' len action -> do
	alexSetInput inp'
	action inp len

scanner str = runAlex str $ scannerAcc []
  where
    scannerAcc i = do
      tok <- alexMonadScan; 
      if tokenClass tok == LEOF
        then return i
        else scannerAcc $! (tok:i)

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

}
