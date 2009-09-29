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
 ,PState() -- need to export this to satisfy haddock
)
where
import Language.CSPM.AST
import Language.CSPM.Token (Token(..),AlexPosn)
import Language.CSPM.TokenClasses as TokenClasses
import qualified Language.CSPM.Token as Token

import qualified Language.CSPM.SrcLoc as SrcLoc
import Language.CSPM.SrcLoc (SrcLoc)

import Language.CSPM.LexHelper (filterIgnoredToken)
import Text.ParserCombinators.Parsec.ExprM

import Text.ParserCombinators.Parsec
  hiding (parse,eof,notFollowedBy,anyToken,label,ParseError,errorPos,token)
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
mkSrcSpan b e= SrcLoc.mkTokSpan b e

{-# DEPRECATED mkSrcPos "simplify alternatives for sourcelocations" #-}
mkSrcPos :: Token -> SrcLoc
mkSrcPos l = SrcLoc.mkTokPos l

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

token :: TokenClasses.PrimToken -> PT ()
token t = tokenPrimExDefault tokenTest
  where
    tokenTest tok = if tokenClass tok == t
      then Just ()
      else Nothing

{-
builtInFunctions :: Set TokenClasses.PrimToken
builtInFunctions = Set.fromList
      [ T_union ,T_inter, T_diff, T_Union, T_Inter,
        T_member, T_card, T_empty, T_set, T_Set,
        T_Seq, T_null, T_head, T_tail, T_concat,
        T_elem, T_length, T_CHAOS ]
-}

anyBuiltIn :: PT Const
anyBuiltIn = try $ do
  tok <- tokenPrimExDefault (\t -> Just $ tokenClass t)
  case tok of
    T_union  -> return F_union
    T_inter  -> return F_inter
    T_diff   -> return F_diff
    T_Union  -> return F_Union
    T_Inter  -> return F_Inter
    T_member -> return F_member
    T_card   -> return F_card
    T_empty  -> return F_empty
    T_set    -> return F_set
    T_Set    -> return F_Set
    T_Seq    -> return F_Seq
    T_null   -> return F_null
    T_head   -> return F_head
    T_tail   -> return F_tail
    T_concat -> return F_concat
    T_elem   -> return F_elem
    T_length -> return F_length
    T_CHAOS  -> return F_CHAOS
    _        -> fail "not a built-in function"


blockBuiltIn :: PT a
blockBuiltIn = do
  bi <- anyBuiltIn
  fail $ "can not use built-in '"++ show bi ++ "' here" -- todo fix: better error -message


lIdent :: PT String
lIdent =
  tokenPrimExDefault testToken
  <?> "identifier"
  where
    testToken t = case tokenClass t of
      L_Ident -> Just $ tokenString t
      _ -> Nothing

ident :: PT LIdent
ident   = withLoc (lIdent >>= return . Ident)

varExp :: PT LExp
varExp= withLoc (ident >>= return . Var)

commaSeperator :: PT ()
commaSeperator = token T_comma

sepByComma :: PT x -> PT [x]
sepByComma a = sepBy a commaSeperator

sepBy1Comma :: PT x -> PT [x]
sepBy1Comma a = sepBy1 a commaSeperator

rangeCloseExp :: PT (LExp,LExp)
rangeCloseExp = do
  s<-parseExp_noPrefix
  token T_dotdot
  e<- parseExp_noPrefix
  return (s,e)

rangeOpenExp :: PT LExp
rangeOpenExp = do
  s <- parseExp_noPrefix
  token T_dotdot
  return s

comprehensionExp :: PT ([LExp],[LCompGen])
comprehensionExp = do
  expList <- sepByComma parseExp
  gens <- parseComprehension
  return (expList,gens)

parseComprehension :: PT [LCompGen]
parseComprehension = token T_mid >> sepByComma (compGenerator<|>compGuard )

compGuard :: PT LCompGen
compGuard= withLoc (parseExp_noPrefix >>= return . Guard)

compGenerator :: PT LCompGen
compGenerator = try $ withLoc $ do
  pat <- parsePattern
  token T_leftarrow
  exp <- parseExp_noPrefix
  return $ Generator pat exp

-- replicated operations use comprehensions with a differen Syntax
comprehensionRep :: PT LCompGenList
comprehensionRep = withLoc $ do
  l <- sepByComma (repGenerator <|> compGuard)
  token T_at
  return l

repGenerator :: PT LCompGen
repGenerator = try $ withLoc $ do
  pat <- parsePattern
  token T_colon
  exp <- parseExp_noPrefix
  return $ Generator pat exp

inBraces :: PT x -> PT x
inBraces = between (token T_openBrace) (token T_closeBrace)

inParens :: PT x -> PT x
inParens = between (token T_openParen) (token T_closeParen)

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
  $ between (token T_openPBrace) (token T_closePBrace) comprehensionExp

-- todo check in csp-m doku size of Int
intLit :: PT Integer
intLit =
   -- " - {-comment-} 10 " is parsed as Integer(-10) "
      (token T_minus >> linteger >>= return . negate)
  <|> linteger
  where 
    linteger :: PT Integer
    linteger = tokenPrimExDefault testToken
    testToken t = if tokenClass t == L_Integer
      then Just $ read $ tokenString t
      else Nothing 

negateExp :: PT LExp
negateExp = withLoc $ do
  token T_minus
  body <- parseExp
  return $ NegExp body

litExp :: PT LExp
litExp = inSpan IntExp intLit

litPat :: PT LPattern
litPat = inSpan IntPat intLit

letExp :: PT LExp
letExp = withLoc $ do
  token T_let
  decl <- parseDeclList
  token T_within
  exp <- parseExp
  return $ Let decl exp            

ifteExp :: PT LExp
ifteExp = withLoc $ do
  token T_if
  cond <- parseExp
  token T_then
  thenExp <- parseExp
  token T_else
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
  fkt <- inSpan BuiltIn anyBuiltIn
  args <- parseFunArgs
  return $ CallBuiltIn fkt args

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
  token T_backslash
  patList <- sepBy1 parsePattern $ token T_comma
  token T_at
  exp <- parseExp
  return $ Lambda patList exp

parseExpBase :: PT LExp
parseExpBase =
         parenExpOrTupleEnum 
     <|> (try funCall)
     <|> withLoc ( token T_STOP >> return Stop)
     <|> withLoc ( token T_SKIP >> return Skip)
     <|> withLoc ( token T_true >> return CTrue)
     <|> withLoc ( token T_false >> return CFalse)
     <|> withLoc ( token T_Events >> return Events)
     <|> withLoc ( token T_Bool >> return BoolSet)
     <|> withLoc ( token T_Int >> return IntSet)
     <|> ifteExp
     <|> letExp
     <|> try litExp       -- -10 is Integer(-10) 
     <|> negateExp        -- -(10) is NegExp(Integer(10))
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
maybe need a Ast-node for parenExp for prettyPrint-Printing
parenExps are now a special case of TupleExps
-}

parenExpOrTupleEnum :: PT LExp
parenExpOrTupleEnum = withLoc $ do
  body <- inParens $ sepByComma parseExp
  case body of
    [] -> return $ TupleExp []
    [x] -> return $ Parens x
    _ -> return  $ TupleExp body


{-
Warning : postfixM and Prefix may not be nested
 "not not true" does not parse !!
-}
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
   ,[ infixM (nfun2 T_hat     F_Concat ) AssocLeft,
     prefixM (nfun1 T_hash    F_Len2 ) -- different from Roscoe Book
    ]
   ,[ infixM (nfun2 T_times   F_Mult ) AssocLeft
     ,infixM (nfun2 T_slash   F_Div ) AssocLeft
     ,infixM (nfun2 T_percent F_Mod  ) AssocLeft
    ]
   ,[ infixM (nfun2 T_plus    F_Add ) AssocLeft,
      infixM (nfun2 T_minus   F_Sub ) AssocLeft
    ]
   ,[ infixM (nfun2 T_eq      F_Eq ) AssocLeft
     ,infixM (nfun2 T_neq     F_NEq) AssocLeft
     ,infixM (nfun2 T_ge      F_GE ) AssocLeft
     ,infixM (nfun2 T_le      F_LE ) AssocLeft
     ,infixM (nfun2 T_lt      F_LT ) AssocLeft
     ,infixM (do
        s <- getNextPos
        gtSym
        e <- getLastPos
        op <- mkLabeledNode (mkSrcSpan s e) (BuiltIn F_GT)
        return $ (\a b-> mkLabeledNode (posFromTo a b) $ Fun2 op a b)
      ) AssocLeft
    ]
   ,[ prefixM ( token T_not >> unOp NotExp )]
   ,[ infixM ( token T_and >> binOp AndExp) AssocLeft ]
   ,[ infixM ( token T_or >> binOp OrExp) AssocLeft ]
   ,[ infixM proc_op_aparallel AssocLeft ]
   ,[ infixM proc_op_lparallel AssocLeft ]

   ,[infixM procOpSharing AssocLeft ]
   ,[infixM (nfun2 T_amp        F_Guard      ) AssocLeft]
   ,[infixM (nfun2 T_semicolon  F_Sequential ) AssocLeft]
   ,[infixM (nfun2 T_triangle   F_Interrupt  ) AssocLeft]
   ,[infixM (nfun2 T_box        F_ExtChoice  ) AssocLeft]
   ,[infixM (nfun2 T_rhd        F_Timeout    ) AssocLeft]
   ,[infixM (nfun2 T_sqcap      F_IntChoice  ) AssocLeft]
   ,[infixM (nfun2 T_interleave F_Interleave ) AssocLeft]
   ,[infixM (nfun2 T_backslash  F_Hiding     ) AssocLeft]
  ] 
  where
  nfun1 :: TokenClasses.PrimToken -> Const -> PT (LExp -> PT LExp)
  nfun1 tok cst = do
    fkt <- biOp tok cst
    pos<-getPos
    return $ (\a -> mkLabeledNode pos $ Fun1 fkt a)

  nfun2 :: TokenClasses.PrimToken -> Const -> PT (LExp -> LExp -> PT LExp)
  nfun2 tok cst = do
    fkt <- biOp tok cst
    pos<-getLastPos 
--   return $ \a b -> mkLabeledNode (posFromTo a b) $ Fun2 fkt a b
    return $ \a b -> mkLabeledNode (mkSrcPos pos) $ Fun2 fkt a b

  binOp :: (LExp -> LExp -> Exp) -> PT (LExp -> LExp -> PT LExp)
  binOp op = return $ \a b -> mkLabeledNode (posFromTo a b) $ op a b

  unOp :: (LExp -> Exp) -> PT (LExp -> PT LExp )
  unOp op = do
    pos<-getLastPos
    return $ (\a -> mkLabeledNode (mkSrcPos pos) $ op a)

  biOp :: TokenClasses.PrimToken -> Const -> PT LBuiltIn
  biOp tok cst = inSpan BuiltIn (token tok >> return cst)

  posFromTo :: LExp -> LExp -> SrcLoc.SrcLoc
  posFromTo a b = SrcLoc.srcLocFromTo (srcLoc a) (srcLoc b)

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
  dotExp <- sepBy1 baseExp $ token T_dot
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
  token T_gt
  updateState countGt  --we count the occurences of gt-symbols
  next <- testFollows parseExp  -- and accept it only if it is followed by an expression
  case next of
    Nothing -> fail "Gt token not followed by an expression"
    (Just _) -> do                 --
      mode <- getStates gtMode
      case mode of
        GtNoLimit -> return ()
        (GtLimit x) -> do
          cnt <- getStates gtCounter
          if cnt < x then return ()
                     else fail "(Gt token belongs to sequence expression)"
{-
parse an sequenceexpression <...>
we have to be carefull not to parse the end of sequence ">"
as comparision
-}

token_gt :: PT ()
token_gt = token T_gt

token_lt :: PT ()
token_lt = token T_lt

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
{-
parse an expression which contains as most count Greater-symbols (">"
used to leave the last ">" as end of sequence
attention: this can be nested !!
-}

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
  token T_openBrack
  a1<-parseExp_noPrefix
  token T_parallel
  a2<-parseExp_noPrefix
  token T_closeBrack
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
  s <- getNextPos
  token T_openBrackBrack
  ren<-(sepBy parseRename commaSeperator)
  gens <- optionMaybe parseComprehension
  token T_closeBrackBrack
  e<-getLastPos
  case gens of
    Nothing -> return $ (\p1 -> mkLabeledNode (mkSrcSpan s e ) $ ProcRenaming ren p1)
    Just g -> return $ (\p1 -> mkLabeledNode (mkSrcSpan s e ) $ ProcRenamingComprehension ren g p1 )

parseLinkList :: PT LLinkList
parseLinkList = withLoc $ do
  token T_openBrack
  linkList<-(sepBy parseLink commaSeperator)
  gens <- optionMaybe parseComprehension
  token T_closeBrack
  case gens of
    Nothing -> return $ LinkList linkList
    Just g -> return $ LinkListComprehension g linkList

parseLink :: PT LLink
parseLink= withLoc $ do
  e1<-parseExp_noPrefix
  token T_leftrightarrow
  e2<-parseExp_noPrefix
  return $ Link e1 e2

parseRename :: PT LRename
parseRename= withLoc $ do
  e1<-parseExp_noPrefix
  token T_leftarrow
  e2<-parseExp_noPrefix
  return $ Rename e1 e2


-- here starts the parser for pattern
parsePatternNoDot :: PT LPattern
parsePatternNoDot = let ?innerDot = False in parsePatternAlso

parsePattern :: PT LPattern
parsePattern = let ?innerDot = True in parsePatternAlso

parsePatternAlso :: (?innerDot::Bool) => PT LPattern
parsePatternAlso = ( do
  sPos <- getNextPos
  concList <- sepBy1 parsePatternAppend $ token T_atat
  ePos <- getLastPos
  case concList of 
    [x] -> return x
    l -> mkLabeledNode  (mkSrcSpan sPos ePos) $ Also l
  ) 
  <?> "pattern"

parsePatternAppend :: (?innerDot::Bool) => PT LPattern
parsePatternAppend = do
  sPos <- getNextPos
  concList <- sepBy1 parsePatternDot $ token T_hat
  ePos <- getLastPos
  case concList of 
    [x] -> return x
    l -> mkLabeledNode (mkSrcSpan sPos ePos) $ Append l

parsePatternDot :: (?innerDot::Bool) => PT LPattern
parsePatternDot = case ?innerDot of
  False -> parsePatternCore
  True -> do
    s <- getNextPos
    dList <- sepBy1 parsePatternCore $ token T_dot
    e <- getLastPos
    case dList of
      [p] -> return p
      l -> mkLabeledNode (mkSrcSpan s e) $ DotPat l

parsePatternCore :: (?innerDot::Bool) => PT LPattern
parsePatternCore =
      nestedPattern
  <|> withLoc ( token T_true >> return TruePat)
  <|> withLoc ( token T_false >> return FalsePat)
  <|> litPat
  <|> varPat
  <|> tuplePatEnum
  <|> listPatEnum
  <|> singleSetPat
  <|> emptySetPat
  <|> withLoc ( token T_underscore >> return WildCard)
  <|> blockBuiltIn
  <?> "pattern"
  where
    nestedPattern = try $ inParens parsePattern

    varPat :: (?innerDot :: Bool) => PT LPattern
    varPat = inSpan VarPat ident

    singleSetPat :: (?innerDot :: Bool) => PT LPattern
    singleSetPat = try $ inSpan SingleSetPat $ inBraces parsePattern

    emptySetPat :: (?innerDot :: Bool) => PT LPattern
    emptySetPat = withLoc ( token T_openBrace >> token T_closeBrace >> return EmptySetPat )

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
    token T_assert
    p1<-parseExp
    op<- token T_Refine {- ToDo: fix this -}
    p2<-parseExp
    return $ AssertRef p1 "k" p2

  assertBool = withLoc $ do
    token T_assert
    b<-parseExp
    return $ AssertBool b

  parseAssert :: PT LDecl
  parseAssert = (try assertRef) <|> assertBool

  parseTransparent :: PT LDecl
  parseTransparent = withLoc $ do
    token T_transparent
    l <- sepBy1Comma ident
    return $ Transparent l

  parseSubtype :: PT LDecl
  parseSubtype = withLoc $ do
    token T_subtype
    i <- ident
    token_is
    conList<-sepBy1 constrDef $ token T_mid
    return $ SubType i conList

  parseDatatype :: PT LDecl
  parseDatatype = withLoc $ do
    token T_datatype
    i <- ident
    token_is
    conList<-sepBy1 constrDef $ token T_mid
    return $ DataType i conList

  constrDef :: PT LConstructor
  constrDef = withLoc $ do
    i <- ident
    ty <- optionMaybe constrType
    return $ Constructor i ty

  constrType = try ( token T_dot >> typeExp)

  parseNametype :: PT LDecl
  parseNametype = withLoc $ do
    token T_nametype
    i <- ident
    token_is
    t<-typeExp
    return $ NameType i t

  parseChannel :: PT LDecl
  parseChannel = withLoc $ do
    token T_channel
    identl<-sepBy1Comma ident
    t<-optionMaybe typeDef
    return $ Channel identl t

  
  typeDef = token T_colon >> typeExp
  typeExp = typeTuple <|> typeDot

  typeTuple = inSpan TypeTuple $ inParens $ sepBy1Comma parseExp

  typeDot = inSpan TypeDot $
    sepBy1 parseExpBase $ token T_dot

  parsePrint :: PT LDecl
  parsePrint = withLoc $ do
    token T_print
    e <- parseExp
    return $ Print e

procOpSharing :: PT (LProc -> LProc -> PT LProc)
procOpSharing = do
  spos <- getNextPos
  al <- between ( token T_openOxBrack) (token T_closeOxBrack) parseExp
  epos <- getLastPos
  return $ (\a b  -> mkLabeledNode (mkSrcSpan spos epos) $ ProcSharing al a b)

closureExp :: PT LExp
closureExp = inSpan Closure $
  between (token T_openPBrace ) (token T_closePBrace)
          (sepBy1Comma parseExp )

{- Replicated Expressions in Prefix form -}

parseProcReplicatedExp :: PT LProc
parseProcReplicatedExp = do
      procRep T_semicolon   ProcRepSequence
  <|> procRep T_sqcap ProcRepInternalChoice
  <|> procRep T_box   ProcRepExternalChoice
  <|> procRep T_interleave ProcRepInterleave
  <|> procRepAParallel
  <|> procRepLinkParallel
  <|> procRepSharing
  <|> parsePrefixExp
  <|> parseExpBase
  <?> "parseProcReplicatedExp"
  where
  -- todo : refactor all these to using inSpan
  procRep :: TokenClasses.PrimToken -> (LCompGenList -> LProc -> Exp) -> PT LProc
  procRep sym fkt = withLoc $ do
    token sym
    l<-comprehensionRep
    body <- parseExp
    return $ fkt l body

  procRepAParallel = withLoc $ do
    token T_parallel
    l<-comprehensionRep
    token T_openBrack
    alph <- parseExp
    token T_closeBrack
    body <- parseExp
    return $ ProcRepAParallel l alph body

  procRepLinkParallel = withLoc $ do
    link <- parseLinkList
    gen <-comprehensionRep
    body <- parseExp
    return $ ProcRepLinkParallel gen link body

  procRepSharing = withLoc $ do
    al <- between (token T_openOxBrack ) (token T_closeOxBrack) parseExp
    gen <- comprehensionRep
    body <- parseExp
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
    token T_rightarrow
    return (channel,commfields)


{-
this is not what fdr really does
fdr parese ch?x.y:a as ch?((x.y):a)
-}
parseCommField :: PT LCommField
parseCommField = inComm <|> outComm <|> dotComm <?> "communication field"
  where
  inComm = withLoc $ do
    token T_questionmark
    updateState$ setLastChannelDir WasIn
    inCommCore

  inCommCore = do
    pat<-parsePatternNoDot
    guarD <- optionMaybe (token T_colon >> parseExp_noPrefix_NoDot)
    case guarD of
      Nothing -> return $ InComm pat
      Just g  -> return $ InCommGuarded pat g

  outComm = withLoc $ do
    token T_exclamation
    updateState $ setLastChannelDir WasOut
    e <- parseExp_noPrefix_NoDot    
    return $ OutComm e

-- repeat the direction of the last CommField
  dotComm = withLoc $ do
    token T_dot
    lastDir <- getStates lastChannelDir
    case lastDir of
      WasOut -> do
         com<-parseExp_noPrefix_NoDot
         return $ OutComm com
      WasIn -> inCommCore




{-
Helper routines for connecting the Token with the parser
and general Helper routines
The following is not related to CSPM-Syntax
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
token_is = token T_is

tokenPrimExDefault :: (Token -> Maybe a) -> GenParser Token PState a
tokenPrimExDefault = tokenPrimEx Token.showToken primExUpdatePos (Just primExUpdateState)