-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Parser
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This modules defines a Parser for CSPM
-- 
-----------------------------------------------------------------------------
{- todo:
* add Autoversion to packet
* add wrappers for functions that throw dynamic exceptions
-}
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE ImplicitParams #-}
module Language.CSPM.Parser
(
  parse
 ,ParseError(..)
)
where
import Language.CSPM.AST
import Language.CSPM.Token (Token(..),AlexPosn)
import Language.CSPM.TokenClasses as TokenClasses
import qualified Language.CSPM.Token as Token

import Language.CSPM.SrcLoc (SrcLoc(..))
import qualified Language.CSPM.SrcLoc as SrcLoc


import Language.CSPM.LexHelper (filterIgnoredToken)
import Text.ParserCombinators.Parsec.ExprM

import Data.Set as Set (fromList,member)
import Text.ParserCombinators.Parsec
  hiding (parse,eof,notFollowedBy,anyToken,label,ParseError,errorPos)
import Text.ParserCombinators.Parsec.Pos (newPos)
import qualified Text.ParserCombinators.Parsec.Error as ParsecError
import Data.Typeable (Typeable)
import Control.Monad.State
import Data.List
import Prelude hiding (exp)
import Control.Exception (Exception)

type PT a= GenParser Token PState a

-- | The 'parse' function parses a List of 'Token'.
-- It returns a 'ParseError' or a 'Labled' 'Module'.
-- The 'SourceName' argument is currently not used.
parse :: 
      SourceName
   -> [Token]
   -> Either ParseError LModule
parse filename tokenList
  = wrapParseError tokenList $
      runParser (parseModule tokenList) initialPState filename $ filterIgnoredToken tokenList

data ParseError = ParseError {
   parseErrorMsg :: String
  ,parseErrorToken :: Token
  ,parseErrorPos   :: AlexPosn
  } deriving (Show,Typeable)

instance Exception ParseError

data PState
 = PState {
  lastTok        :: Token
 ,lastChannelDir :: LastChannelDir
 ,gtCounter      :: Int
 ,gtMode         :: GtMode
 ,nodeIdSupply   :: NodeId
 } deriving Show

initialPState :: PState
initialPState = PState {
   lastTok = Token.tokenSentinel 
  ,lastChannelDir = WasOut
  ,gtCounter = 0
  ,gtMode = GtNoLimit
  ,nodeIdSupply = mkNodeId 0
  }

setLastChannelDir :: LastChannelDir -> PState -> PState 
setLastChannelDir dir env = env {lastChannelDir=dir}

setGtMode :: GtMode-> PState -> PState
setGtMode mode env = env {gtMode = mode}

countGt :: PState -> PState
countGt env = env {gtCounter = gtCounter env +1 }

data LastChannelDir = WasIn | WasOut deriving Show
data GtMode=GtNoLimit | GtLimit Int deriving Show

instance NodeIdSupply (GenParser Token PState) where
  getNewNodeId = do
    i <- gets nodeIdSupply
    modify $ \s -> s { nodeIdSupply = succ $ nodeIdSupply s}
    return i  

instance MonadState PState (GenParser Token PState) where
  get = getState
  put = setState

getNextPos :: PT Token
getNextPos = do
  tokenList <-getInput
  case tokenList of
    (hd:_) -> return hd
    [] -> return Token.tokenSentinel

getLastPos :: PT Token
getLastPos = getStates lastTok

getPos :: PT SrcLoc
getPos = do
  t<-getNextPos 
  return $ mkSrcPos t

mkSrcSpan :: Token -> Token -> SrcLoc
mkSrcSpan b e= TokSpan b e

mkSrcPos :: Token -> SrcLoc
mkSrcPos l = TokPos l

withLoc :: PT a -> PT (Labeled a)
withLoc a = do
  s <- getNextPos
  av <- a
  e <- getLastPos
  mkLabeledNode (mkSrcSpan s e) av

inSpan :: (a -> b ) -> PT a -> PT (Labeled b) 
inSpan constr exp = do
  s <- getNextPos
  l <- exp
  e <- getLastPos
  mkLabeledNode (mkSrcSpan s e) $ constr l

parseModule :: [Token.Token] -> PT (Labeled Module)
parseModule tokenList = withLoc $ do
 decl<-topDeclList 
 eof <?> "end of module"
 return $ Module {
   moduleDecls = decl
  ,moduleTokens = Just tokenList
  }

linteger :: PT String
linteger = 
  mytoken ( \tok -> case tok of
                      (L_Integer,s) -> Just s
                      _ -> Nothing )

symbol :: TokenClasses.Symbol -> PT ()
symbol sym =
  mytoken ( \tok -> case tok of
                      (L_Symbol s,_)   | s == sym  -> Just ()
                      _ -> Nothing )

keyword :: Keyword -> PT ()
keyword k = 
  mytoken ( \tok -> case tok of
                      (L_Keyword x,_)   | x == k  -> Just ()
                      _ -> Nothing )


anyBuiltIn :: PT TokenClasses.BuiltIn
anyBuiltIn = 
  mytoken ( \tok -> case tok of
                      (L_BuiltIn b ,_) -> Just b
                      _ -> Nothing )

builtIn :: TokenClasses.BuiltIn -> PT ()
builtIn b = 
  mytoken ( \tok -> case tok of
                      (L_BuiltIn x,_)   | x == b  -> Just ()
                      _ -> Nothing )

blockBuiltIn :: PT a
blockBuiltIn = do
  bi <- anyBuiltIn
  fail $ "can not use built-in '"++ show bi ++ "' here" -- todo fix: better error -message


lIdent :: PT String
lIdent= 
  mytoken ( \tok -> case tok of
                      (L_Ident,s) -> Just s
                      _ -> Nothing )
  <?> "identifier"

ident :: PT LIdent
ident   = withLoc (lIdent >>= return . Ident)

varExp :: PT LExp
varExp= withLoc (ident >>= return . Var)

commaSeperator :: PT ()
commaSeperator = symbol T_comma

sepByComma :: PT x -> PT [x]
sepByComma a = sepBy a commaSeperator

sepBy1Comma :: PT x -> PT [x]
sepBy1Comma a = sepBy1 a commaSeperator

rangeCloseExp :: PT (LExp,LExp)
rangeCloseExp = do
  s<-parseExp_noPrefix
  symbol T_dotdot
  e<- parseExp_noPrefix
  return (s,e)

rangeOpenExp :: PT LExp
rangeOpenExp = do
  s <- parseExp_noPrefix
  symbol T_dotdot
  return s

comprehensionExp :: PT ([LExp],[LCompGen])
comprehensionExp = do
  expList <- sepByComma parseExp
  gens <- parseComprehension
  return (expList,gens)

parseComprehension :: PT [LCompGen]
parseComprehension = symbol T_mid >> sepByComma (compGenerator<|>compGuard )

compGuard :: PT LCompGen
compGuard= withLoc (parseExp_noPrefix >>= return . Guard)

compGenerator :: PT LCompGen
compGenerator = try $ withLoc $ do
  pat <- parsePattern
  symbol T_leftarrow
  exp <- parseExp_noPrefix
  return $ Generator pat exp

-- replicated operations use comprehensions with a differen Syntax
comprehensionRep :: PT LCompGenList
comprehensionRep = withLoc $ do
  l <- sepByComma (repGenerator <|> compGuard)
  symbol T_at
  return l

repGenerator :: PT LCompGen
repGenerator = try $ withLoc $ do
  pat <- parsePattern
  symbol T_dot
  exp <- parseExp_noPrefix
  return $ Generator pat exp

inBraces :: PT x -> PT x
inBraces = between (symbol T_openBrace) (symbol T_closeBrace)

inParens :: PT x -> PT x
inParens = between (symbol T_openParen) (symbol T_closeParen)

setExpEnum :: PT LExp
setExpEnum =   inSpan SetEnum $ inBraces  (sepByComma parseExp) 

listExpEnum :: PT LExp
listExpEnum =  inSpan ListEnum $ betweenLtGt (sepByComma parseExp_noPrefix)

setExpOpen :: PT LExp
setExpOpen  =  inSpan SetOpen  $ inBraces rangeOpenExp 

listExpOpen :: PT LExp
listExpOpen =  inSpan ListOpen $ betweenLtGt rangeOpenExp

setExpClose :: PT LExp
setExpClose =  inSpan SetClose $ inBraces rangeCloseExp

listExpClose :: PT LExp
listExpClose =  inSpan ListClose $ betweenLtGt rangeCloseExp 

setComprehension :: PT LExp
setComprehension =  inSpan SetComprehension $ inBraces comprehensionExp

listComprehension :: PT LExp
listComprehension =  inSpan  ListComprehension $ betweenLtGt comprehensionExp

closureComprehension :: PT LExp
closureComprehension = inSpan  ClosureComprehension
  $ between (symbol T_openPBrace) (symbol T_closePBrace) comprehensionExp

-- todo check in csp-m doku size of Int
intLit :: PT Integer
intLit = ( do
   symbol T_minus
   val <- linteger
   return  ( - (read val :: Integer) )
  )
 <|>
   ( do
     val <- linteger
     return (read val :: Integer)
   )

litExp :: PT LExp
litExp = inSpan IntExp intLit

litPat :: PT LPattern
litPat = inSpan IntPat intLit

letExp :: PT LExp
letExp = withLoc $ do
  keyword T_let
  decl <- parseDeclList
  keyword T_within
  exp <- parseExp
  return $ Let decl exp            

ifteExp :: PT LExp
ifteExp = withLoc $ do
  keyword T_if
  cond <- parseExp
  keyword T_then
  thenExp <- parseExp
  keyword T_else
  elseExp <- parseExp
  return $ Ifte cond thenExp elseExp


funCall :: PT LExp
funCall = funCallFkt <|> funCallBi

funCallFkt :: PT LExp
funCallFkt = withLoc $ do
  fkt <- varExp 
  args <- parseFunArgs
  return $ CallFunction fkt args

funCallBi :: PT LExp
funCallBi = withLoc $ do
  fkt <- builtIn
  args <- parseFunArgs
  return $ CallBuiltIn fkt args
  where 
    builtIn :: PT LBuiltIn
    builtIn = withLoc $ do
      b <- anyBuiltIn
      if Set.member b properFunctions
        then return $ BuiltIn b
        else fail "not a callable function"
    properFunctions = Set.fromList
      [ T_union ,T_inter, T_diff, T_Union, T_Inter,
        T_member, T_card, T_empty, T_set, T_Set,
        T_Seq, T_null, T_head, T_tail, T_concat,
        T_elem, T_length, T_CHAOS ]

parseFunArgs :: PT [[LExp]]
parseFunArgs =  do
  argsL <- many1 funArgsT
  return argsL

{-
fun application in tuple form f(1,2,3)
if the tuple is follwed by "=", it  belongs to the next declaration
g = h
(a,b) = (1,2)
-}

funArgsT :: PT [LExp]
funArgsT = try $ do
   tArgs <- inParens $ sepByComma parseExp
   notFollowedBy' token_is
   return tArgs

lambdaExp :: PT LExp
lambdaExp = withLoc $ do
  symbol T_backslash
  patList <- sepBy1 parsePattern $ symbol T_comma
  symbol T_at
  exp <- parseExp
  return $ Lambda patList exp

parseExpBase :: PT LExp
parseExpBase =
         parenExpOrTupleEnum 
     <|> (try funCall)
     <|> withLoc ( builtIn T_STOP >> return Stop)
     <|> withLoc ( builtIn T_SKIP >> return Skip)
     <|> withLoc ( builtIn T_true >> return CTrue)
     <|> withLoc ( builtIn T_false >> return CFalse)
     <|> withLoc ( builtIn T_Events >> return Events)
     <|> withLoc ( builtIn T_Bool >> return BoolSet)
     <|> withLoc ( builtIn T_Int >> return IntSet)
     <|> ifteExp
     <|> letExp
     <|> litExp
     <|> varExp
     <|> lambdaExp
     <|> try closureComprehension
     <|> closureExp
     <|> try listComprehension
     <|> try setComprehension
     <|> try listExpEnum
     <|> try setExpEnum
     <|> try setExpClose
     <|> try listExpClose
     <|> try setExpOpen
     <|> try listExpOpen
     <|> blockBuiltIn
     <?> "core-expression" 


{-
--maybe need a Ast-node for parenExp for prettyPrint-Printing
parenExps are now a special case of TupleExps
-}

parenExpOrTupleEnum :: PT LExp
parenExpOrTupleEnum = withLoc $ do
  body <- inParens $ sepByComma parseExp
  case body of
    [] -> return $ TupleExp []
    [x] -> return $ Parens x
    _ -> return  $ TupleExp body


-- Warning : postfixM and Prefix may not be nested
-- "not not true" does not parse !!
opTable :: [[Text.ParserCombinators.Parsec.ExprM.Operator
                                           Token PState LExp]]
opTable =
   [
--   [ infixM ( cspSym "." >> binOp mkDotPair) AssocRight ]
--   ,
-- dot.expression moved to a seperate Step
-- ToDo : fix funApply and procRenaming
    [ postfixM funApplyImplicit ]
   ,[ postfixM procRenaming ]
   ,[ infixM (nfun2Sym T_hat T_Concat ) AssocLeft,
      prefixM (nfun1 T_Len2 ) -- different from Roscoe Book
    ]
   ,[ infixM (nfun2 T_Mult ) AssocLeft
     ,infixM (nfun2 T_Div ) AssocLeft
     ,infixM (nfun2 T_Mod  ) AssocLeft
    ]
   ,[ infixM (nfun2 T_Add ) AssocLeft,
      infixM (nfun2Sym "-" T_Sub ) AssocLeft
    ]
   ,[ infixM (nfun2 T_Eq ) AssocLeft
     ,infixM (nfun2 T_NEq ) AssocLeft
     ,infixM (nfun2 T_GE ) AssocLeft
     ,infixM (nfun2 T_LE ) AssocLeft
     ,infixM (nfun2Sym "<" T_LT ) AssocLeft
     ,infixM (do
        gtSym
        op <- mkLabeledNode NoLocation (BuiltIn T_GT)  -- ToDo : fix this
        pos <- getPos
        return $ (\a b-> mkLabeledNode pos $ Fun2 op a b)
      ) AssocLeft
    ]
   ,[ prefixM ( builtIn T_not >> unOp NotExp )]
   ,[ infixM ( builtIn T_and >> binOp AndExp) AssocLeft ]
   ,[ infixM ( builtIn T_or >> binOp OrExp) AssocLeft ]
   ,[ infixM proc_op_aparallel AssocLeft ]
   ,[ infixM proc_op_lparallel AssocLeft ]

   ,[infixM procOpSharing AssocLeft ]
   ,[infixM (nfun2 T_Guard ) AssocLeft]
   ,[infixM (nfun2 T_Semicolon ) AssocLeft]
   ,[infixM (nfun2 T_Interrupt ) AssocLeft]
   ,[infixM (nfun2 T_ExtChoice ) AssocLeft]
   ,[infixM (nfun2 T_Timeout ) AssocLeft]
   ,[infixM (nfun2 T_IntChoice ) AssocLeft]
   ,[infixM (nfun2 T_Interleave ) AssocLeft]
   ,[infixM (nfun2Sym T_backslash T_Hiding) AssocLeft]
  ] 
  where
  nfun1 :: TokenClasses.BuiltIn -> PT (LExp -> PT LExp)
  nfun1 op = do
    fkt <- biOp op
    pos<-getPos
    return $ (\a -> mkLabeledNode pos $ Fun1 fkt a)

  nfun2 :: TokenClasses.Symbol -> PT (LExp -> LExp -> PT LExp)
  nfun2 op = do
    pos<-getPos
    fkt <- biOp op
    return $ (\a b -> mkLabeledNode pos $ Fun2 fkt a b)
  nfun2Sym :: TokenClasses.Symbol -> TokenClasses.BuiltIn -> PT (LExp -> LExp -> PT LExp)
  nfun2Sym s t = do
    symbol s
    op <- mkLabeledNode NoLocation (BuiltIn t)  -- ToDo : fix this
    pos <- getPos
    return $ (\a b-> mkLabeledNode pos $ Fun2 op a b)

  binOp :: (LExp -> LExp -> Exp) -> PT (LExp -> LExp -> PT LExp)
  binOp op = do
    pos<-getLastPos
    return $ (\a b-> mkLabeledNode (mkSrcPos pos) $ op a b)

  unOp :: (LExp -> Exp) -> PT (LExp -> PT LExp )
  unOp op = do
    pos<-getLastPos
    return $ (\a -> mkLabeledNode (mkSrcPos pos) $ op a)

  biOp :: TokenClasses.BuiltIn -> PT LBuiltIn
  biOp op = inSpan BuiltIn (builtIn op  >> return op)

parseExp :: PT LExp
parseExp =
  (parseDotExpOf $
    buildExpressionParser opTable parseProcReplicatedExp
  )
  <?> "expression"

parseExp_noPrefix_NoDot :: PT LExp
parseExp_noPrefix_NoDot = buildExpressionParser opTable parseExpBase

parseExp_noPrefix :: PT LExp
parseExp_noPrefix = parseDotExpOf parseExp_noPrefix_NoDot

parseDotExpOf :: PT LExp -> PT LExp
parseDotExpOf baseExp = do
  sPos <-getNextPos
  dotExp <- sepBy1 baseExp $ symbol T_dot
  ePos <-getLastPos
  case dotExp of 
     [x] -> return x
     l -> mkLabeledNode (mkSrcSpan sPos ePos) $ DotTuple l

{-
place (term) as a suffix behind any term to
make a function application
used a lot in CspBook -examples
notice : we do not destict between f(a,b,c) and f(a)(b)(c) or f(a,b)(c)
this is buggy for f(a)(b)(c)
this may interact with normal function -application !
-}
funApplyImplicit :: PT (LExp -> PT LExp)
funApplyImplicit = do
  args <- parseFunArgs
  pos <-getPos
  return $ (\fkt -> mkLabeledNode pos $ CallFunction fkt args )


-- this is complicated and meight as well be buggy !
gtSym :: PT ()
gtSym = try $ do
  symbol T_gt
  updateState countGt  --we count the occurences of gt-symbols
  next <- testFollows parseExp  -- and accept it only if it is followed by an expression
  case next of
    Nothing -> fail "Gt Symbol not followed by an expression"
    (Just _) -> do                 --
      mode <- getStates gtMode
      case mode of
        GtNoLimit -> return ()
        (GtLimit x) -> do
          cnt <- getStates gtCounter
          if cnt < x then return ()
                     else fail "(Gt Symbol belongs to sequence expression)"
{-
parse an sequenceexpression <...>
we have to be carefull not to parse the end of sequence ">"
as comparision
-}

token_gt :: PT ()
token_gt = symbol T_gt

token_lt :: PT ()
token_lt = symbol T_lt

betweenLtGt :: PT a -> PT a
betweenLtGt parser = do
  token_lt
  st <- getParserState  -- maybe we need to backtrack
  body <- parser           -- even if this is successfull
  cnt <- getStates gtCounter
  endSym <-testFollows token_gt
  case endSym of
    Just () -> do
      token_gt
      return body   -- gtSym could make distinction between endOfSequence and GtSym
    Nothing -> do  -- last comparision expression was indeed end of sequence
      setParserState st --backtrack
      s <- parseWithGtLimit (cnt) parser
      token_gt
      return s

--parse an expression which contains as most count Greater-symbols (">"
--used to leave the last ">" as end of sequence
--attention: this can be nested !!

parseWithGtLimit :: Int -> PT a -> PT a
parseWithGtLimit maxGt parser = do
  oldLimit <- getStates gtMode
  updateState $ setGtMode $ GtLimit maxGt
  res <- optionMaybe parser
  updateState $ setGtMode oldLimit
  case res of
    Just p -> return p
    Nothing -> fail "contents of sequence expression"

proc_op_aparallel :: PT (LExp -> LExp -> PT LExp)
proc_op_aparallel = try $ do
  s <- getNextPos
  symbol T_openBrack
  a1<-parseExp_noPrefix
  symbol T_parallel
  a2<-parseExp_noPrefix
  symbol T_closeBrack
  e<-getLastPos
  return $ (\p1 p2 -> mkLabeledNode (mkSrcSpan s e ) $ ProcAParallel a1 a2 p1 p2 )

proc_op_lparallel :: PT (LExp -> LExp -> PT LExp)
proc_op_lparallel = try $ do
  ren <- parseLinkList
  p <- getPos
  return $ (\p1 p2 -> mkLabeledNode p $ ProcLinkParallel ren p1 p2)

procRenaming :: PT (LExp -> PT LExp)
procRenaming = do
  rens <- many1 procOneRenaming
  return $ (\x -> foldl (>>=) (return x) rens)

procOneRenaming :: PT (LExp -> PT LExp )
procOneRenaming = try $ do
  symbol T_openBrackBrack
  ren<-(sepBy parseRename commaSeperator)
  gens <- optionMaybe parseComprehension
  symbol T_closeBrackBrack
  p<-getPos
  case gens of
    Nothing -> return $ (\p1 -> mkLabeledNode p $ ProcRenaming ren p1)
    Just g -> return $ (\p1 -> mkLabeledNode p $ ProcRenamingComprehension ren g p1 )

parseLinkList :: PT LLinkList
parseLinkList = withLoc $ do
  symbol T_openBrack
  linkList<-(sepBy parseLink commaSeperator)
  gens <- optionMaybe parseComprehension
  symbol T_closeBrack
  case gens of
    Nothing -> return $ LinkList linkList
    Just g -> return $ LinkListComprehension g linkList

parseLink :: PT LLink
parseLink= withLoc $ do
  e1<-parseExp_noPrefix
  symbol T_leftrightarrow
  e2<-parseExp_noPrefix
  return $ Link e1 e2

parseRename :: PT LRename
parseRename= withLoc $ do
  e1<-parseExp_noPrefix
  symbol T_leftarrow
  e2<-parseExp_noPrefix
  return $ Rename e1 e2


-- here start the parser for pattern
parsePatternNoDot :: PT LPattern
parsePatternNoDot = let ?innerDot = False in parsePatternAlso

parsePattern :: PT LPattern
parsePattern = let ?innerDot = True in parsePatternAlso

parsePatternAlso :: (?innerDot::Bool) => PT LPattern
parsePatternAlso = ( do
  sPos <- getNextPos
  concList <- sepBy1 parsePatternAppend (symbol T_atat)
  ePos <- getLastPos
  case concList of 
    [x] -> return x
    l -> mkLabeledNode  (mkSrcSpan sPos ePos) $ Also l
  ) 
  <?> "pattern"

parsePatternAppend :: (?innerDot::Bool) => PT LPattern
parsePatternAppend = do
  sPos <- getNextPos
  concList <- sepBy1 parsePatternDot (symbol T_hat)
  ePos <- getLastPos
  case concList of 
    [x] -> return x
    l -> mkLabeledNode (mkSrcSpan sPos ePos) $ Append l

parsePatternDot :: (?innerDot::Bool) => PT LPattern
parsePatternDot = case ?innerDot of
  False -> parsePatternCore
  True -> do
    s <- getNextPos
    dList <- sepBy1 parsePatternCore (symbol T_dot)
    e <- getLastPos
    case dList of
      [p] -> return p
      l -> mkLabeledNode (mkSrcSpan s e) $ DotPat l

parsePatternCore :: (?innerDot::Bool) => PT LPattern
parsePatternCore =
      nestedPattern
  <|> withLoc ( builtIn T_true >> return TruePat)
  <|> withLoc ( builtIn T_false >> return FalsePat)
  <|> litPat
  <|> varPat
  <|> tuplePatEnum
  <|> listPatEnum
  <|> singleSetPat
  <|> emptySetPat
  <|> withLoc ( symbol T_underscore >> return WildCard)
  <|> blockBuiltIn
  <?> "pattern"
  where
    nestedPattern = try $ inParens parsePattern

    varPat :: (?innerDot :: Bool) => PT LPattern
    varPat = inSpan VarPat ident

    singleSetPat :: (?innerDot :: Bool) => PT LPattern
    singleSetPat = try $ inSpan SingleSetPat $ inBraces parsePattern

    emptySetPat :: (?innerDot :: Bool) => PT LPattern
    emptySetPat = withLoc ( symbol T_openBrace >> symbol T_closeBrace >> return EmptySetPat )

    listPatEnum :: (?innerDot :: Bool) => PT LPattern
    listPatEnum =  inSpan ListEnumPat $ between token_lt token_gt (sepByComma parsePattern)

    tuplePatEnum :: (?innerDot :: Bool) => PT LPattern
    tuplePatEnum = inSpan TuplePat $ inParens (sepByComma parsePattern)


-- FixMe: do not use patBind to parse variable bindings ?

patBind :: PT LDecl
patBind = withLoc $ do
  pat <- parsePattern
  token_is
  exp <-parseExp
  return $ PatBind pat exp

-- parse all fundefs and merge consecutive case alternatives
funBind :: PT [LDecl]
funBind = do
  flist <-many1 sfun
  -- group functioncases by the name of the function
  let flgr = groupBy
              (\a b -> (unIdent $ unLabel $ fst $ a) == (unIdent $ unLabel $ fst b))
              flist
  mapM mkFun flgr
  where 
     mkFun :: [(LIdent,(FunArgs,LExp))] -> PT LDecl
     mkFun l = do
        let
          fname = fst $ head l
          pos = srcLoc fname
          cases = map ((uncurry FunCase) . snd  ) l
        mkLabeledNode pos $ FunBind fname cases

-- parse a single function-case
sfun :: PT (LIdent,(FunArgs,LExp))
sfun = do
  (fname,patl) <- try sfunHead
  token_is <?> "rhs of function clause"
  exp <-parseExp
  return (fname,(patl,exp))
  where
    sfunHead = do    
      fname <- ident
      patl <- parseFktCurryPat
      return (fname,patl)

{-
in CSP f(x)(y), f(x,y) , f((x,y)) are all different
we parse a function pattern as a list of curryargs (a)(b)( )( )..
each of with can be a comma-seperated list of args that do not allow
currying in-between
i,e (a,b,c)(d,e,f) -> [[a,b,c][d,e,f]]
-}

parseFktCurryPat :: PT [[LPattern]]
parseFktCurryPat = many1 parseFktCspPat

parseFktCspPat :: PT [LPattern]
parseFktCspPat = inParens $ sepByComma parsePattern

{-
5. nov 2007 remove try to give better error-messages
parseFktCspPat = 
  try $ between (cspSym "(") (cspSym ")") $ sepByComma parsePattern
todo: better error-messages for fun(card) (card is a buildin)
-}

{-
parsePatL = withLoc $ between (cspSym "(") (cspSym ")")
                     $ sepBy parsePattern funArgumentSeperator
funArgumentSeperator = cspSym "," <|> (try (cspSym ")" >> cspSym "("))
-}

parseDeclList :: PT [LDecl]
parseDeclList = do
  decl<- many1 parseDecl
  return $ concat decl


singleList :: PT a -> PT [a]
singleList a = do
  av <-a
  return [av]

{-
returns a list of decls
because funBind can't easily parse a single function
ToDo : PatBinds are actually different from varbind
example x={} with patbind will not be polymorphic
-}
parseDecl :: PT [LDecl]
parseDecl =
       funBind
   <|> singleList patBind 
   <?> "declaration"

topDeclList :: PT [LDecl]
topDeclList = do
  decl<- many1 topDecl
  return $ concat decl
  where

  topDecl :: PT [LDecl]
  topDecl =
        funBind
    <|> singleList patBind
    <|> singleList parseAssert
    <|> singleList parseTransparent
    <|> singleList parseDatatype
    <|> singleList parseSubtype
    <|> singleList parseNametype
    <|> singleList parseChannel
    <|> singleList parsePrint
    <?> "top-level declaration"   

  assertRef = withLoc $ do
    keyword T_assert
    p1<-parseExp
{- todo:
fix this according to scattergood
    op<-     (cspSym "[T=" >> return "[T=" )
         <|> (cspSym "[F=" >> return "[F=" )
         <|> (cspSym "[FD=" >> return "[FD=" )
-}
    op <- return "k"
    p2<-parseExp
    return $ AssertRef p1 op p2

  assertBool = withLoc $ do
    keyword T_assert
    b<-parseExp
    return $ AssertBool b

  parseAssert :: PT LDecl
  parseAssert = (try assertRef) <|> assertBool

  parseTransparent :: PT LDecl
  parseTransparent = withLoc $ do
    keyword T_transparent
    l <- sepBy1Comma ident
    return $ Transparent l

  parseSubtype :: PT LDecl
  parseSubtype = withLoc $ do
    keyword T_subtype
    i <- ident
    token_is
    conList<-sepBy1 constrDef $ symbol T_mid
    return $ SubType i conList

  parseDatatype :: PT LDecl
  parseDatatype = withLoc $ do
    keyword T_datatype
    i <- ident
    token_is
    conList<-sepBy1 constrDef $ symbol T_mid
    return $ DataType i conList

  constrDef :: PT LConstructor
  constrDef = withLoc $ do
    i <- ident
    ty <- optionMaybe constrType
    return $ Constructor i ty

  constrType = try ( symbol T_dot >> typeExp)

  parseNametype :: PT LDecl
  parseNametype = withLoc $ do
    keyword T_nametype
    i <- ident
    token_is
    t<-typeExp
    return $ NameType i t

  parseChannel :: PT LDecl
  parseChannel = withLoc $ do
    keyword T_channel
    identl<-sepBy1Comma ident
    t<-optionMaybe typeDef
    return $ Channel identl t

  
  typeDef = symbol T_colon >> typeExp
  typeExp = typeTuple <|> typeDot

  typeTuple = inSpan TypeTuple $ inParens $ sepBy1Comma parseExp

  typeDot = inSpan TypeDot $
    sepBy1 parseExpBase (symbol T_dot)

  parsePrint :: PT LDecl
  parsePrint = withLoc $ do
    keyword T_print
    e <- parseExp
    return $ Print e

procOpSharing :: PT (LProc -> LProc -> PT LProc)
procOpSharing = do
  spos <- getNextPos
  al <- between ( symbol T_openOxBrack) (symbol T_closeOxBrack) parseExp
  epos <- getLastPos
  return $ (\a b  -> mkLabeledNode (mkSrcSpan spos epos) $ ProcSharing al a b)

closureExp :: PT LExp
closureExp = inSpan Closure $
  between (symbol T_openPBrace ) (symbol T_closePBrace)
          (sepBy1Comma parseExp )

{- Replicated Expressions in Prefix form -}

parseProcReplicatedExp :: PT LProc
parseProcReplicatedExp = do
      procRep T_semicolon   ProcRepSequence
  <|> procRep T_sqcap ProcRepInternalChoice
  <|> procRep T_interleave ProcRepInterleave
  <|> procRep T_box  ProcRepChoice
  <|> procRepAParallel
  <|> procRepLinkParallel
  <|> procRepSharing
  <|> parsePrefixExp
  <|> parseExpBase
  <?> "parseProcReplicatedExp"
  where
  -- todo : refactor all these to using inSpan
  procRep :: TokenClasses.Symbol -> (LCompGenList -> LProc -> Exp) -> PT LProc
  procRep sym fkt = withLoc $ do
    symbol sym
    l<-comprehensionRep
    body <- parseProcReplicatedExp
    return $ fkt l body

  procRepAParallel = withLoc $ do
    symbol T_parallel
    l<-comprehensionRep
    symbol T_openBrack
    alph <- parseExp
    symbol T_closeBrack
    body <- parseProcReplicatedExp
    return $ ProcRepAParallel l alph body

  procRepLinkParallel = withLoc $ do
    link <- parseLinkList
    gen<-comprehensionRep
    body <- parseProcReplicatedExp
    return $ ProcRepLinkParallel gen link body

  procRepSharing = withLoc $ do
    al <- between ( symbol T_openOxBrack ) (symbol T_closeOxBrack) parseExp
    gen <- comprehensionRep
    body <- parseProcReplicatedExp
    return $ ProcRepSharing gen al body

{-
parsePrefixExp is to be called without try
i.e. it must only commit after "->"

prefix binds stronger than any operator (except dot-operator)
either another prefix or an expression without prefix
exp <-(parsePrefixExp <|> parseExpBase ) <?> "rhs of prefix operation"

-}
parsePrefixExp :: PT LExp
parsePrefixExp = withLoc $ do
  (channel,comm) <- try parsePrefix
  exp <- parseProcReplicatedExp <?> "rhs of prefix operation"
  return $ PrefixExp channel comm exp
  where 
  parsePrefix :: PT (LExp,[LCommField])
  parsePrefix = do
    channel <- try funCall <|> varExp --maybe permit even more
    updateState $ setLastChannelDir WasOut
    commfields <- many parseCommField
    symbol T_rightarrow
    return (channel,commfields)


{-
this is not what fdr really does
fdr parese ch?x.y:a as ch?((x.y):a)
-}
parseCommField :: PT LCommField
parseCommField = inComm <|> outComm <|> dotComm <?> "communication field"
  where
  inComm = withLoc $ do
    symbol T_questionmark
    updateState$ setLastChannelDir WasIn
    inCommCore

  inCommCore = do
    pat<-parsePatternNoDot
    guarD <- optionMaybe (symbol T_colon >> parseExp_noPrefix_NoDot)
    case guarD of
      Nothing -> return $ InComm pat
      Just g  -> return $ InCommGuarded pat g

  outComm = withLoc $ do
    symbol T_exclamation
    updateState $ setLastChannelDir WasOut
    e <- parseExp_noPrefix_NoDot    
    return $ OutComm e

-- repeat the direction of the last CommField
  dotComm = withLoc $ do
    symbol T_dot
    lastDir <- getStates lastChannelDir
    case lastDir of
      WasOut -> do
         com<-parseExp_noPrefix_NoDot
         return $ OutComm com
      WasIn -> inCommCore




{-
Helper routines for connecting the Token with the parser
and general Helper routines
Te following is not related to CSPM-Syntax
-}


--maybe this is Combinator.lookAhead ?

testFollows :: PT x -> PT (Maybe x)
testFollows p = do
  oldState <- getParserState
  res<-optionMaybe p
  setParserState oldState
  return res

getStates :: (PState -> x) -> PT x
getStates sel = do
  st <- getState
  return $ sel st


primExUpdatePos :: SourcePos -> Token -> t -> SourcePos
primExUpdatePos pos t@(Token {}) _
  = newPos (sourceName pos) (-1) (Token.unTokenId $ Token.tokenId t)

primExUpdateState :: t -> Token -> t1 -> PState -> PState
primExUpdateState _ tok _ st = st { lastTok =tok}

{-
replicating existing combinators, just to work with our lexer
improve this
-}

anyToken :: PT Token
anyToken = tokenPrimEx Token.showToken primExUpdatePos (Just primExUpdateState) Just

notFollowedBy :: PT Token -> PT ()
notFollowedBy p  = try (do{ c <- p; unexpected $ Token.showToken c }
                       <|> return ()
                       )

notFollowedBy' :: PT x -> PT ()
notFollowedBy' p  = try (do{ p; pzero }
                       <|> return ()
                       )

eof :: PT ()
eof  = notFollowedBy anyToken <?> "end of input"

mytoken :: ((TokenClass, String) -> Maybe a) -> PT a
mytoken test = tokenPrimEx Token.showToken primExUpdatePos (Just primExUpdateState) testToken
  where testToken t@(Token {}) = test (tokenClass t, tokenString t)

pprintParsecError :: ParsecError.ParseError -> String
pprintParsecError err
  = ParsecError.showErrorMessages "or" "unknown parse error" 
      "expecting" "unexpected" "end of input"
        (ParsecError.errorMessages err)


wrapParseError :: [Token] -> Either ParsecError.ParseError LModule -> Either ParseError LModule
wrapParseError _ (Right ast) = Right ast
wrapParseError tl (Left err) = Left $ ParseError {
   parseErrorMsg = pprintParsecError err
  ,parseErrorToken = errorTok
  ,parseErrorPos = tokenStart errorTok
  }
  where 
    tokId = Token.mkTokenId $ sourceColumn $ ParsecError.errorPos err
    errorTok = maybe Token.tokenSentinel id  $ find (\t -> tokenId t ==  tokId) tl


token_is :: PT ()
token_is = symbol T_is
