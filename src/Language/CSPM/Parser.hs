-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Parser
-- Copyright   :  (c) Fontaine 2008 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de, me@dobrikov.biz
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This modules defines a Parser for CSP-M
-- 
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Language.CSPM.Parser
(
  parse
 ,ParseError(..)
 ,testParser
 ,parseExp
 ,parsePattern
)
where
import Language.CSPM.AST
import Language.CSPM.Token (Token(..),AlexPosn)
import Language.CSPM.TokenClasses as TokenClasses
import qualified Language.CSPM.Token as Token

import qualified Language.CSPM.SrcLoc as SrcLoc
import Language.CSPM.SrcLoc (SrcLoc)

import Language.CSPM.LexHelper (removeIgnoredToken)
import Text.ParserCombinators.Parsec.ExprM

import Text.ParserCombinators.Parsec
  hiding (parse,eof,notFollowedBy,anyToken,label,ParseError,errorPos,token,newline)
import Text.ParserCombinators.Parsec.Pos (newPos)
import qualified Text.ParserCombinators.Parsec.Error as ParsecError
import Data.Typeable (Typeable)
import Control.Monad.State
import Data.List
import Data.Maybe
import Prelude hiding (exp)
import Control.Exception (Exception)

type PT a = GenParser Token PState a

-- | The 'parse' function parses a List of 'Token'.
-- It returns a 'ParseError' or a 'Labled' 'Module'.
-- The 'SourceName' argument is currently not used.
parse :: 
      SourceName
   -> [Token]
   -> Either ParseError ModuleFromParser
parse filename tokenList
  = wrapParseError tokenList $
      runParser
        (parseModule tokenList)
        initialPState
        filename
        (removeIgnoredToken tokenList)

-- | Wrapper for testing sub parsers
testParser :: PT a -> [Token] -> Either ParsecError.ParseError a
testParser p tokenList
  = runParser p initialPState ""
       $ removeIgnoredToken tokenList

-- | ParseError data type. This is an instance of Excpetion
data ParseError = ParseError {
   parseErrorMsg :: String
  ,parseErrorToken :: Token
  ,parseErrorPos   :: AlexPosn
  } deriving (Show,Typeable)

instance Exception ParseError

data PState
 = PState {
  lastTok        :: Token
 ,gtCounter      :: Int
 ,gtLimit        :: Maybe Int
 ,nodeIdSupply   :: NodeId
 } deriving Show

initialPState :: PState
initialPState = PState {
   lastTok = Token.tokenSentinel 
  ,gtCounter = 0
  ,gtLimit = Nothing
  ,nodeIdSupply = mkNodeId 0
  }

mkLabeledNode :: SrcLoc -> t -> PT (Labeled t)
mkLabeledNode loc node = do
  i <- getStates nodeIdSupply
  updateState $ \s -> s { nodeIdSupply = succ $ nodeIdSupply s}
  return $ Labeled {
    nodeId = i
   ,srcLoc = loc
   ,unLabel = node }

getStates :: (PState -> x) -> PT x
getStates sel = do
  st <- getState
  return $ sel st

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
mkSrcSpan b e = SrcLoc.mkTokSpan b e

{-# DEPRECATED mkSrcPos "simplify alternatives for sourcelocations" #-}
mkSrcPos :: Token -> SrcLoc
mkSrcPos l = SrcLoc.mkTokPos l

withLoc :: PT a -> PT (Labeled a)
withLoc a = do
  s <- getNextPos
  av <- a
  e <- getLastPos
  mkLabeledNode (mkSrcSpan s e) av

inSpan :: (a -> b) -> PT a -> PT (Labeled b) 
inSpan constr exp = do
  s <- getNextPos
  l <- exp
  e <- getLastPos
  mkLabeledNode (mkSrcSpan s e) $ constr l

parseModule :: [Token] -> PT ModuleFromParser
parseModule tokenList = do
  s <- getNextPos
  skipMany newline
  moduleDecls <- topDeclList
  eof <?> "end of module"
  e <- getLastPos
  let
    moduleTokens = Just tokenList
    moduleSrcLoc = mkSrcSpan s e
    modulePragmas = mapMaybe getPragma tokenList
    moduleComments = mapMaybe getComment tokenList
  return $ Module { .. }
  where
    getComment :: Token -> Maybe LocComment
    getComment t = case tokenClass t of
      L_LComment -> Just (LineComment str, loc)
      L_BComment -> Just (BlockComment str, loc)
      L_Pragma -> Just (PragmaComment str, loc)
      _ -> Nothing
      where
        loc = mkSrcPos t
        str = tokenString t
    getPragma :: Token -> Maybe String
    getPragma t = case tokenClass t of
      L_Pragma -> Just $ take (tokenLen t - 6) $ drop 3 $ tokenString t
      _ -> Nothing

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

newline :: PT ()
newline = token L_Newline

refineOp :: PT LRefineOp
refineOp = withLoc $ do 
  tok <- tokenPrimExDefault (\t -> Just $ tokenClass t)
  case tok of
    T_trace  -> return Trace
    T_failure  -> return Failure
    T_failureDivergence   -> return FailureDivergence
    T_refusalTesting -> return RefusalTesting
    T_refusalTestingDiv -> return RefusalTestingDiv
    T_revivalTesting -> return RevivalTesting
    T_revivalTestingDiv -> return RevivalTestingDiv
    T_tauPriorityOp -> return TauPriorityOp
    _              -> fail "Unexpected Token"
  
anyBuiltIn :: PT Const
anyBuiltIn = do
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
  bi <- try anyBuiltIn
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

sepByNewLine :: PT x -> PT [x]
sepByNewLine d = sepBy d newline

parseComprehension :: PT [LCompGen]
parseComprehension = token T_mid >> sepByComma (compGenerator <|> compGuard )

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
  where
    repGenerator :: PT LCompGen
    repGenerator = try $ withLoc $ do
      pat <- parsePattern
      (token T_colon) <|> (token T_leftarrow)
      exp <- parseExp_noPrefix
      return $ Generator pat exp

inBraces :: PT x -> PT x
inBraces = between (token T_openBrace) (token T_closeBrace)

inParens :: PT x -> PT x
inParens = between (token T_openParen) (token T_closeParen)

setExp :: PT LExp
setExp = withLoc $ inBraces $ do
  (range,comp) <- lsBody
  return $ SetExp range comp

listExp :: PT LExp
listExp = withLoc $ betweenLtGt $ do
  (range,comp) <- lsBody
  return $ ListExp range comp

lsBody :: PT (LRange, Maybe [LCompGen])
lsBody = liftM2 (,) parseRangeExp (optionMaybe parseComprehension)
  where
    parseRangeExp :: PT LRange
    parseRangeExp = withLoc (rangeClosed <|> rangeOpen <|> rangeEnum)

    rangeEnum = liftM RangeEnum $ sepByComma parseExp_noPrefix

    rangeClosed :: PT Range
    rangeClosed = try $ do
      s <-parseExp_noPrefix
      token T_dotdot
      e <- parseExp_noPrefix
      return $ RangeClosed s e

    rangeOpen :: PT Range
    rangeOpen = try $ do
      s <- parseExp_noPrefix
      token T_dotdot
      return $ RangeOpen s

closureExp :: PT LExp
closureExp = withLoc $ do
  token T_openPBrace
  expList <- sepByComma parseExp
  gens <- optionMaybe $ parseComprehension
  token T_closePBrace
  case gens of
    Nothing -> return $ Closure expList
    Just l -> return  $ ClosureComprehension (expList,l)

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
  declList <- sepByNewLine (funBind <|> patBind)
  token T_within
  exp <- parseExp
  return $ Let declList exp            

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
funCall = try (funCallFkt <|> funCallBi)
  where
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
     <|> funCall
     <|> withLoc ( token T_STOP >> return Stop)
     <|> withLoc ( token T_SKIP >> return Skip)
     <|> withLoc ( token T_true >> return CTrue)
     <|> withLoc ( token T_false >> return CFalse)
     <|> withLoc ( token T_Events >> return Events)
     <|> withLoc ( token T_Bool >> return BoolSet)
     <|> withLoc ( token T_Int >> return IntSet)
     <|> withLoc ( token T_Proc >> return ProcSet)
     <|> ifteExp
     <|> letExp
     <|> try litExp       -- -10 is Integer(-10) 
     <|> negateExp        -- -(10) is NegExp(Integer(10))
     <|> varExp
     <|> lambdaExp
     <|> closureExp
     <|> listExp
     <|> setExp
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
Warning :
the expression parser does not accept nested Postfix and Prefix expressions
 "not not true" does not parse !!
-}
type OpTable = [[Text.ParserCombinators.Parsec.ExprM.Operator Token PState LExp]]
opTable :: OpTable
opTable = baseTable ++ procTable

baseTable :: OpTable
procTable :: OpTable
(baseTable, procTable) = (
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
   ],
   [[ infixM proc_op_aparallel AssocLeft ]
   ,[ infixM proc_op_lparallel AssocLeft ]

   ,[infixM procOpSharing AssocLeft ]
   ,[infixM (nfun2 T_backslash  F_Hiding     ) AssocLeft]
   ,[infixM (nfun2 T_amp        F_Guard      ) AssocLeft]
   ,[infixM (nfun2 T_semicolon  F_Sequential ) AssocLeft]
   ,[infixM (nfun2 T_triangle   F_Interrupt  ) AssocLeft]
   ,[infixM (nfun2 T_box        F_ExtChoice  ) AssocLeft]
   ,[infixM (nfun2 T_rhd        F_Timeout    ) AssocLeft]
   ,[infixM (nfun2 T_sqcap      F_IntChoice  ) AssocLeft]
   ,[infixM procOpException AssocLeft]
   ,[infixM (nfun2 T_interleave F_Interleave ) AssocLeft]
  ]
  )
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

  procOpSharing :: PT (LProc -> LProc -> PT LProc)
  procOpSharing = try $ do
    spos <- getNextPos
    al <- between ( token T_openOxBrack) (token T_closeOxBrack) parseExp
    epos <- getLastPos
    return $ (\a b  -> mkLabeledNode (mkSrcSpan spos epos) $ ProcSharing al a b)

  procOpException :: PT (LProc -> LProc -> PT LProc)
  procOpException = do
    spos <- getNextPos
    al <- between ( token T_openOxBrack) (token T_exp) parseExp
    epos <- getLastPos
    return $ (\a b  -> mkLabeledNode (mkSrcSpan spos epos) $ ProcException al a b)

{-
We count the occurences of gt-symbols
and accept it only if it is followed by an expression.
If a gtLimit is set, we only accept a maximum number of gt symbols
-}
  gtSym :: PT ()
  gtSym = try $ do
    token T_gt
    updateState (\env -> env {gtCounter = gtCounter env +1 })
    next <- testFollows parseExp
    case next of
      Nothing -> fail "Gt token not followed by an expression"
      Just _  -> do
        mode <- getStates gtLimit
        case mode of
          Nothing -> return ()
          Just x  -> do
            cnt <- getStates gtCounter
            if cnt < x then return ()
                       else fail "(Gt token belongs to sequence expression)"

-- | Parser for CSP-M expressions
parseExp :: PT LExp
parseExp
  = (parseDotExpOf $ buildExpressionParser procTable parseProcReplicatedExp)
  <?> "expression"


-- todo : check if we need parseExp_noPrefix or if we can use parseExp_noProc
parseExp_noPrefix :: PT LExp
parseExp_noPrefix = parseDotExpOf parseExp_noPrefix_NoDot
   where
     parseExp_noPrefix_NoDot :: PT LExp
     parseExp_noPrefix_NoDot = buildExpressionParser opTable parseExpBase

-- todo :: parseExpBase does include STOP and SKIP 
parseExp_noProc :: PT LExp
parseExp_noProc
  = parseDotExpOf $ buildExpressionParser baseTable parseExpBase

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
      _ <- setParserState st --backtrack
      s <- parseWithGtLimit (cnt) parser
      token_gt
      return s
{-
parse an expression which contains as most count Greater-symbols (">"
the last ">" is left as end of sequence
attention: this can be nested !!
-}

parseWithGtLimit :: Int -> PT a -> PT a
parseWithGtLimit maxGt parser = do
  oldLimit <- getStates gtLimit
  setGtLimit $ Just maxGt
  res <- optionMaybe parser
  setGtLimit oldLimit
  case res of
    Just p -> return p
    Nothing -> fail "contents of sequence expression"
  where
    setGtLimit g = updateState $ \env -> env {gtLimit = g}

proc_op_aparallel :: PT (LExp -> LExp -> PT LExp)
proc_op_aparallel = try $ do
  s <- getNextPos
  token T_openBrack
  a1 <- parseExp_noPrefix
  token T_parallel
  a2 <- parseExp_noPrefix
  token T_closeBrack
  e <- getLastPos
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
  gens <- optionMaybe $ withLoc parseComprehension
  token T_closeBrackBrack
  e<-getLastPos
  return $ (\p1 -> mkLabeledNode (mkSrcSpan s e ) $ ProcRenaming ren gens p1 )

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

-- | Parser for CSP-M patterns
parsePattern :: PT LPattern
parsePattern = (<?> "pattern")  $ do
  sPos <- getNextPos
  concList <- sepBy1 parsePatternDot $ token T_atat
  ePos <- getLastPos
  case concList of 
    [x] -> return x
    l -> mkLabeledNode  (mkSrcSpan sPos ePos) $ Also l

parsePatternAppend :: PT LPattern
parsePatternAppend = do
  sPos <- getNextPos
  concList <- sepBy1 parsePatternCore $ token T_hat
  ePos <- getLastPos
  case concList of 
    [x] -> return x
    l -> mkLabeledNode (mkSrcSpan sPos ePos) $ Append l

parsePatternDot :: PT LPattern
parsePatternDot = do
  s <- getNextPos
  dList <- sepBy1 parsePatternAppend $ token T_dot
  e <- getLastPos
  case dList of
      [p] -> return p
      l -> mkLabeledNode (mkSrcSpan s e) $ DotPat l

parsePatternCore :: PT LPattern
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
    varPat = inSpan VarPat ident
    singleSetPat = try $ inSpan SingleSetPat $ inBraces parsePattern
    emptySetPat = withLoc ( token T_openBrace >> token T_closeBrace >> return EmptySetPat )
    listPatEnum =  inSpan ListEnumPat $ between token_lt token_gt (sepByComma parsePattern)
    tuplePatEnum = inSpan TuplePat $ inParens (sepByComma parsePattern)


-- FixMe: do not use patBind to parse variable bindings ?

patBind :: PT LDecl
patBind = withLoc $ do
  pat <- parsePattern
  token_is
  exp <-parseExp
  return $ PatBind pat exp

-- parse a single function-case
funBind :: PT LDecl
funBind = try $ do
  fname <- ident
  patl <- parseFktCurryPat
  token_is <?> "rhs of function clause"
  exp <-parseExp
  mkLabeledNode (srcLoc fname) $ FunBind fname [FunCase patl exp]

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

topDeclList :: PT [LDecl]
topDeclList = sepByNewLine topDecl 
 where
  topDecl :: PT LDecl
  topDecl = choice
      [
        funBind
      , patBind
      , parseAssertDecl
      , parseTransparent
      , parseDatatype
      , parseSubtype
      , parseNametype
      , parseChannel
      , parsePrint
      ] <?> "top-level declaration"

  assertPolarity = fmap (odd . length) $ many $ token T_not

  assertListRef = withLoc $ do
    token T_assert
    negated <- assertPolarity
    p1 <- parseExp
    op <- refineOp
    p2 <- parseExp
    return $ AssertRefine negated p1 op p2

  assertBool = withLoc $ do
    token T_assert
    b <- parseExp
    return $ AssertBool b

  assertTauPrio = withLoc $ do
    token T_assert
    negated <- assertPolarity
    p1 <- parseExp
    op <- tauRefineOp
    p2 <- parseExp
    token T_openAssertBrack
    token T_tau
    token T_priority
    optional $ token T_over
    token T_closeAssertBrack
    set <- parseExp
    return $ AssertTauPrio negated p1 op p2 set
     where
      tauRefineOp :: PT LTauRefineOp
      tauRefineOp = withLoc $ do 
        tok <- tokenPrimExDefault (\t -> Just $ tokenClass t)
        case tok of
         T_trace  -> return TauTrace
         T_Refine -> return TauRefine
         _        -> fail "Unexpected Token"

  assertIntFDRChecks = withLoc $ do
    token T_assert
    negated <- assertPolarity
    p       <- parseExp
    token T_openAssertBrack
    model   <- fdrModel
    extmode <- many $ extsMode
    ext     <-  case extmode of
               []   -> return Nothing
               [x]  -> return $ Just x
               _    -> fail "More than one model extension."
    token T_closeSpecialBrack
    return $ AssertModelCheck negated p model ext
      where
       fdrModel :: PT LFDRModels
       fdrModel = withLoc $ do
        tok <- tokenPrimExDefault (\t -> Just $ tokenClass t)
        case tok of 
         T_deadlock  -> token T_free >> return DeadlockFree
         T_deterministic -> return Deterministic
         T_livelock  -> token T_free >> return LivelockFree
         _ -> fail "Modus is not supported by this parser."
  
       extsMode :: PT LFdrExt
       extsMode =  withLoc $ tokenPrimExDefault test
         where 
          test tok = case tokenClass tok of
                  T_F   -> Just F
                  T_FD  -> Just FD
                  T_T   -> Just T
                  _     -> Nothing

  parseAssert :: PT LAssertDecl
  parseAssert =  try assertTauPrio
             <|> try assertIntFDRChecks
             <|> try assertListRef
             <|> assertBool
             <?> "assert Declaration"

  parseAssertDecl :: PT LDecl
  parseAssertDecl = withLoc $ do
    e <- parseAssert
    return $ Assert e

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
    t <- typeExp
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

  typeDot = inSpan TypeDot $ sepBy1 parseExpBase $ token T_dot

  parsePrint :: PT LDecl
  parsePrint = withLoc $ do
    token T_print
    e <- parseExp
    return $ Print e

{- Replicated Expressions in Prefix form -}

parseProcReplicatedExp :: PT LProc
parseProcReplicatedExp
  = choice
    [ 
      procRep T_semicolon   ProcRepSequence
    , procRep T_sqcap ProcRepInternalChoice
    , procRep T_box   ProcRepExternalChoice
    , procRep T_interleave ProcRepInterleave
    , procRepAParallel
    , procRepLinkParallel
    , procRepSharing
    , parsePrefixExp
    ] <?> "parseProcReplicatedExp"
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
parsePrefixExp either parses a prefix, or just an regular expression

prefix binds stronger than any operator (except dot-operator)
either another prefix or an expression without prefix
exp <-(parsePrefixExp <|> parseExpBase ) <?> "rhs of prefix operation"

-}
parsePrefixExp :: PT LExp
parsePrefixExp = do
  spos <- getNextPos
  start <- parseExp_noProc -- channel or just an expression
  rest <- parsePrefix
  epos <- getLastPos
  case rest of
    Nothing -> return start
    Just (comm,body) -> mkLabeledNode (mkSrcSpan spos epos) $
                           PrefixExp start comm body
  where 
  parsePrefix :: PT (Maybe ([LCommField],LExp))
  parsePrefix = optionMaybe $ do
    commfields <- many parseCommField
    token T_rightarrow
    exp <- parseProcReplicatedExp <?> "rhs of prefix operation"
    return (commfields,exp)


{-
this is not what fdr really does
fdr parese ch?x.y:a as ch?((x.y):a)
-}
parseCommField :: PT LCommField
parseCommField = inComm <|> outComm <?> "communication field"
  where
  inComm = withLoc $ do
    token T_questionmark
    pat<-parsePattern
    mguard <- optionMaybe (token T_colon >> parseExp_noProc)
--    mguard <- optionMaybe (token T_colon >> parseExp_noPrefix_NoDot)
    case mguard of
      Nothing -> return $ InComm pat
      Just g  -> return $ InCommGuarded pat g

  outComm = withLoc $ do
    token T_exclamation
    e <- parseExp_noProc
    return $ OutComm e


{-
Helper routines for connecting the Token with the parser
and general Helper routines
The following is not related to CSPM-Syntax
-}


--maybe this is Combinator.lookAhead ?

testFollows :: PT x -> PT (Maybe x)
testFollows p = do
  oldState <- getParserState
  res <-optionMaybe p
  _ <- setParserState oldState
  return res

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

notFollowedBy :: GenParser tok st Token -> GenParser tok st ()
notFollowedBy p 
  = try (do{ c <- p; unexpected $ Token.showToken c }
         <|> return ()
        )

notFollowedBy' :: GenParser tok st a -> GenParser tok st ()
notFollowedBy' p
  = try $ ( p >> pzero ) <|> return ()

eof :: PT ()
eof  = notFollowedBy anyToken <?> "end of input"

pprintParsecError :: ParsecError.ParseError -> String
pprintParsecError err
  = ParsecError.showErrorMessages "or" "unknown parse error" 
      "expecting" "unexpected" "end of input"
        (ParsecError.errorMessages err)

wrapParseError ::
     [Token]
  -> Either ParsecError.ParseError ModuleFromParser
  -> Either ParseError ModuleFromParser
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
