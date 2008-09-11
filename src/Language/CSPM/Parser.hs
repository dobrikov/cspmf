{- todo:
* add Autoversion to packet
* add wrappers for functions that throw dynamic exceptions
-}
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE ImplicitParams #-}
module Language.CSPM.Parser
(
  parseCSP
 ,ParseError(..)
)
where
import Language.CSPM.AST

import Language.CSPM.Token as Token
  (Token(..),TokenClass(..),tokenSentinel, showToken, unTokenId,AlexPosn,mkTokenId )

import Language.CSPM.LexHelper (filterIgnoredToken)
import Text.ParserCombinators.Parsec.ExprM
import Text.ParserCombinators.Parsec hiding (eof,notFollowedBy,anyToken,label,ParseError,errorPos)
import Text.ParserCombinators.Parsec.Pos (newPos)
import qualified Text.ParserCombinators.Parsec.Error as ParsecError
import Data.Typeable (Typeable)
import Control.Monad.State
import Data.List
import Prelude hiding (exp)

type PT a= GenParser Token PState a

parseCSP :: SourceName -> [Token] -> Either ParseError LModule
parseCSP filename tokenList
  = wrapParseError tokenList $
      runParser (parseModule tokenList) initialPState filename $ filterIgnoredToken tokenList

data ParseError = ParseError {
   errorMsg :: String
  ,errorToken :: Token
  ,errorPos   :: AlexPosn
  } deriving (Show,Typeable)

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
mkSrcSpan b e= TokSpan (Token.tokenId b) (Token.tokenId e)

mkSrcPos :: Token -> SrcLoc
mkSrcPos l = TokPos $ Token.tokenId l

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
                      (LInteger,s) -> Just s
                      _ -> Nothing )

cspSym :: String -> PT ()
cspSym name =
  mytoken ( \tok -> case tok of
                      (LCspsym,s)   | s == name  -> Just ()
                      _ -> Nothing )

cspKey :: String -> PT ()
cspKey name= 
  mytoken ( \tok -> case tok of
                      (LCspId,s)   | s == name  -> Just ()
                      _ -> Nothing )

cspBI :: PT String
cspBI = 
  mytoken ( \tok -> case tok of
                      (LCspBI,s) -> Just s
                      _ -> Nothing )


blockBuiltIn :: PT a
blockBuiltIn = do
  bi <- cspBI
  fail $ "can not use built-in '"++ bi ++ "' here"


lIdent :: PT String
lIdent= 
  mytoken ( \tok -> case tok of
                      (LIdent,s) -> Just s
                      _ -> Nothing )
  <?> "identifier"

ident :: PT LIdent
ident   = withLoc (lIdent >>= return . Ident)

varExp :: PT LExp
varExp= withLoc (ident >>= return . Var)

commaSeperator :: PT ()
commaSeperator = cspSym ","

sepByComma :: PT x -> PT [x]
sepByComma a = sepBy a commaSeperator

sepBy1Comma :: PT x -> PT [x]
sepBy1Comma a = sepBy1 a commaSeperator

rangeCloseExp :: PT (LExp,LExp)
rangeCloseExp = do
  s<-parseExp_noPrefix
  cspSym ".."
  e<- parseExp_noPrefix
  return (s,e)

rangeOpenExp :: PT LExp
rangeOpenExp = do
  s <- parseExp_noPrefix
  cspSym ".."
  return s

comprehensionExp :: PT ([LExp],[LCompGen])
comprehensionExp = do
  expList <- sepByComma parseExp
  gens <- parseComprehension
  return (expList,gens)

parseComprehension :: PT [LCompGen]
parseComprehension = cspSym "|" >> sepByComma (compGenerator<|>compGuard )

compGuard :: PT LCompGen
compGuard= withLoc (parseExp_noPrefix >>= return . Guard)

compGenerator :: PT LCompGen
compGenerator = try $ withLoc $ do
  pat <- parsePattern
  cspSym "<-"
  exp <- parseExp_noPrefix
  return $ Generator pat exp

-- replicated operations use comprehensions with a differen Syntax
comprehensionRep :: PT [LCompGen]
comprehensionRep = do
  l <- sepByComma (repGenerator <|> compGuard)
  cspSym "@"
  return l

repGenerator :: PT LCompGen
repGenerator = try $ withLoc $ do
  pat <- parsePattern
  cspSym ":"
  exp <- parseExp_noPrefix
  return $ Generator pat exp

setExpEnum :: PT LExp
setExpEnum =   inSpan SetEnum $ between (cspSym "{") (cspSym "}") (sepByComma parseExp) 

listExpEnum :: PT LExp
listExpEnum =  inSpan ListEnum $ betweenLtGt (sepByComma parseExp_noPrefix)

setExpOpen :: PT LExp
setExpOpen  =  inSpan SetOpen  $ between (cspSym "{") (cspSym "}") rangeOpenExp 

listExpOpen :: PT LExp
listExpOpen =  inSpan ListOpen $ betweenLtGt rangeOpenExp

setExpClose :: PT LExp
setExpClose =  inSpan SetClose $ between (cspSym "{") (cspSym "}") rangeCloseExp

listExpClose :: PT LExp
listExpClose =  inSpan ListClose $ betweenLtGt rangeCloseExp 

setComprehension :: PT LExp
setComprehension =  inSpan SetComprehension $ between (cspSym "{") (cspSym "}") comprehensionExp

listComprehension :: PT LExp
listComprehension =  inSpan  ListComprehension $ betweenLtGt comprehensionExp

closureComprehension :: PT LExp
closureComprehension = inSpan  ClosureComprehension $ between (cspSym "{|") (cspSym "|}") comprehensionExp

-- todo check in csp-m doku size of Int
intLit :: PT Integer
intLit = ( do
   cspSym "-"
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
  cspKey "let"
  decl <- parseDeclList
  cspKey "within"
  exp <- parseExp
  return $ Let decl exp            

ifteExp :: PT LExp
ifteExp = withLoc $ do
  cspKey "if"
  cond <- parseExp
  cspKey "then"
  thenExp <- parseExp
  cspKey "else"
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


builtIn :: PT LBuiltIn
builtIn = inSpan BuiltIn cspBI

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
   tArgs <- between (cspSym "(") (cspSym ")") $ sepByComma parseExp
   notFollowedBy' $ cspSym "="
   return tArgs

lambdaExp :: PT LExp
lambdaExp = withLoc $ do
  cspSym "\\"
  patList <- sepBy1 parsePattern $ cspSym ","
  cspSym "@"
  exp <- parseExp
  return $ Lambda patList exp

parseExpBase :: PT LExp
parseExpBase =
         parenExpOrTupleEnum 
     <|> (try funCall)
     <|> withLoc ( cspKey "STOP" >> return Stop)
     <|> withLoc ( cspKey "SKIP" >> return Skip)
     <|> withLoc ( cspKey "true" >> return CTrue)
     <|> withLoc ( cspKey "false" >> return CFalse)
     <|> withLoc ( cspKey "Events" >> return Events)
     <|> withLoc ( cspKey "Bool" >> return BoolSet)
     <|> withLoc ( cspKey "Int" >> return IntSet)
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
  body <- between (cspSym "(") (cspSym ")") (sepByComma parseExp)
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
   ,[ infixM (nfun2 "^" ) AssocLeft,
      prefixM (nfun1 "#" ) -- different from Roscoe Book
    ]
   ,[ infixM (nfun2 "*" ) AssocLeft
     ,infixM (nfun2 "/" ) AssocLeft
     ,infixM (nfun2 "%"  ) AssocLeft
    ]
   ,[ infixM (nfun2 "+" ) AssocLeft,
      infixM (nfun2 "-" ) AssocLeft
    ]
   ,[ infixM (nfun2 "==" ) AssocLeft
     ,infixM (nfun2 "!=" ) AssocLeft
     ,infixM (nfun2 ">=" ) AssocLeft
     ,infixM (nfun2 "<=" ) AssocLeft
     ,infixM (nfun2 "<" ) AssocLeft
     ,infixM (do
        gtSym
        pos <- getPos
        return $ (\a b-> mkLabeledNode pos $ Fun2 ">" a b)
      ) AssocLeft
    ]
   ,[ prefixM ( cspKey "not" >> unOp NotExp )]
   ,[ infixM ( cspKey "and" >> binOp AndExp) AssocLeft ]
   ,[ infixM ( cspKey "or" >> binOp OrExp) AssocLeft ]
   ,[ infixM proc_op_aparallel AssocLeft ]
   ,[ infixM proc_op_lparallel AssocLeft ]
{-
   these have moved to a sperate level of recursion
   ,[Prefix $ procRep ";"   "repSequence"
    ,Prefix $ procRep "|~|" "repInternalChoice" 
    ,Prefix $ procRep "|||" "repInterleave"
    ,Prefix $ procRep "[]"  "repChoice"
    ,Prefix procRepAParallel
    ,Prefix procRepLinkParallel
    ,Prefix procRepSharing
    ]
-}
   ,[infixM procOpSharing AssocLeft ]
   ,[infixM (nfun2 "&" ) AssocLeft]
   ,[infixM (nfun2 ";" ) AssocLeft]
   ,[infixM (nfun2 "/\\" ) AssocLeft]
   ,[infixM (nfun2 "[]" ) AssocLeft]
   ,[infixM (nfun2 "[>" ) AssocLeft]
   ,[infixM (nfun2 "|~|" ) AssocLeft]
   ,[infixM (nfun2 "|||" ) AssocLeft]
   ,[infixM (nfun2 "\\" ) AssocLeft]
  ] 
  where
  nfun1 :: String -> PT (LExp -> PT LExp)
  nfun1 op = do
    cspSym op
    pos<-getPos
    return $ (\a -> mkLabeledNode pos $ Fun1 op a)

  nfun2 :: String -> PT (LExp -> LExp -> PT LExp)
  nfun2 op = do
    pos<-getPos
    cspSym op
    return $ (\a b -> mkLabeledNode pos $ Fun2 op a b)

  binOp :: (LExp -> LExp -> Exp) -> PT (LExp -> LExp -> PT LExp)
  binOp op = do
    pos<-getLastPos
    return $ (\a b-> mkLabeledNode (mkSrcPos pos) $ op a b)

  unOp :: (LExp -> Exp) -> PT (LExp -> PT LExp )
  unOp op = do
    pos<-getLastPos
    return $ (\a -> mkLabeledNode (mkSrcPos pos) $ op a)


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
  dotExp <- sepBy1 baseExp $ cspSym "."
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
  cspSym ">"
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
betweenLtGt :: PT a -> PT a
betweenLtGt parser = do
  cspSym "<"
  st <- getParserState  -- maybe we need to backtrack
  body <- parser           -- even if this is successfull
  cnt <- getStates gtCounter
  endSym <-testFollows $ cspSym ">"
  case endSym of
    Just () -> do
      cspSym ">"
      return body   -- gtSym could make distinction between endOfSequence and GtSym
    Nothing -> do  -- last comparision expression was indeed end of sequence
      setParserState st --backtrack
      s <- parseWithGtLimit (cnt) parser
      cspSym ">"
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
  cspSym "["
  a1<-parseExp_noPrefix
  cspSym "||"
  a2<-parseExp_noPrefix
  cspSym "]"
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
  cspSym "[["
  ren<-(sepBy parseRename (cspSym ","))
  gens <- optionMaybe parseComprehension
  cspSym "]]"
  p<-getPos
  case gens of
    Nothing -> return $ (\p1 -> mkLabeledNode p $ ProcRenaming ren p1)
    Just g -> return $ (\p1 -> mkLabeledNode p $ ProcRenamingComprehension ren g p1 )

parseLinkList :: PT LLinkList
parseLinkList = withLoc $ do
  cspSym "["
  linkList<-(sepBy parseLink (cspSym ","))
  gens <- optionMaybe parseComprehension
  cspSym "]"
  case gens of
    Nothing -> return $ LinkList linkList
    Just g -> return $ LinkListComprehension g linkList

parseLink :: PT LLink
parseLink= withLoc $ do
  e1<-parseExp_noPrefix
  cspSym "<->"
  e2<-parseExp_noPrefix
  return $ Link e1 e2

parseRename :: PT LRename
parseRename= withLoc $ do
  e1<-parseExp_noPrefix
  cspSym "<-"
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
  concList <- sepBy1 parsePatternAppend (cspSym "@@")
  ePos <- getLastPos
  case concList of 
    [x] -> return x
    l -> mkLabeledNode  (mkSrcSpan sPos ePos) $ Also l
  ) 
  <?> "pattern"

parsePatternAppend :: (?innerDot::Bool) => PT LPattern
parsePatternAppend = do
  sPos <- getNextPos
  concList <- sepBy1 parsePatternDot (cspSym "^")
  ePos <- getLastPos
  case concList of 
    [x] -> return x
    l -> mkLabeledNode (mkSrcSpan sPos ePos) $ Append l

parsePatternDot :: (?innerDot::Bool) => PT LPattern
parsePatternDot = case ?innerDot of
  False -> parsePatternCore
  True -> do
    s <- getNextPos
    dList <- sepBy1 parsePatternCore (cspSym ".")
    e <- getLastPos
    case dList of
      [p] -> return p
      l -> mkLabeledNode (mkSrcSpan s e) $ DotPat l

parsePatternCore :: (?innerDot::Bool) => PT LPattern
parsePatternCore =
      nestedPattern
  <|> withLoc ( cspKey "true" >> return TruePat)
  <|> withLoc ( cspKey "false" >> return FalsePat)
  <|> litPat
  <|> varPat
  <|> tuplePatEnum
  <|> listPatEnum
  <|> singleSetPat
  <|> emptySetPat
  <|> withLoc ( cspSym "_" >> return WildCard)
  <|> blockBuiltIn
  <?> "pattern"
  where
    nestedPattern = try $ between (cspSym "(") (cspSym ")") parsePattern

    varPat :: (?innerDot :: Bool) => PT LPattern
    varPat = inSpan VarPat ident

    singleSetPat :: (?innerDot :: Bool) => PT LPattern
    singleSetPat = try $ inSpan SingleSetPat $ between (cspSym "{") (cspSym "}") parsePattern

    emptySetPat :: (?innerDot :: Bool) => PT LPattern
    emptySetPat = withLoc ( cspSym "{" >> cspSym "}" >> return EmptySetPat )

    listPatEnum :: (?innerDot :: Bool) => PT LPattern
    listPatEnum =  inSpan ListEnumPat $ between (cspSym "<") (cspSym ">") (sepByComma parsePattern)

    tuplePatEnum :: (?innerDot :: Bool) => PT LPattern
    tuplePatEnum = inSpan TuplePat $ between (cspSym "(") (cspSym ")") (sepByComma parsePattern)


-- FixMe: do not use patBind to parse variable bindings ?

patBind :: PT LDecl
patBind = withLoc $ do
  pat <- parsePattern
  cspSym "="
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
  cspSym "=" <?> "rhs of function clause"
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
parseFktCspPat = between (cspSym "(") (cspSym ")") $ sepByComma parsePattern

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
    cspKey "assert"
    p1<-parseExp
    op<-     (cspSym "[T=" >> return "[T=" )
         <|> (cspSym "[F=" >> return "[F=" )
         <|> (cspSym "[FD=" >> return "[FD=" )
    p2<-parseExp
    return $ AssertRef p1 op p2

  assertBool = withLoc $ do
    cspKey "assert"
    b<-parseExp
    return $ AssertBool b

  parseAssert :: PT LDecl
  parseAssert = (try assertRef) <|> assertBool

  parseTransparent :: PT LDecl
  parseTransparent = withLoc $ do
    cspKey "transparent"
    l <- sepBy1Comma ident
    return $ Transparent l

  parseSubtype :: PT LDecl
  parseSubtype = withLoc $ do
    cspKey "subtype"
    i <- ident
    cspSym "="
    conList<-sepBy1 constrDef (cspSym "|")
    return $ SubType i conList

  parseDatatype :: PT LDecl
  parseDatatype = withLoc $ do
    cspKey "datatype"
    i <- ident
    cspSym "="
    conList<-sepBy1 constrDef (cspSym "|")
    return $ DataType i conList

  constrDef :: PT LConstructor
  constrDef = withLoc $ do
    i <- ident
    ty <- optionMaybe constrType
    return $ Constructor i ty

  constrType = try ( cspSym "." >> typeExp)

  parseNametype :: PT LDecl
  parseNametype = withLoc $ do
    cspKey "nametype"
    i <- ident
    cspSym "="
    t<-typeExp
    return $ NameType i t

  parseChannel :: PT LDecl
  parseChannel = withLoc $ do
    cspKey "channel"
    identl<-sepBy1Comma ident
    t<-optionMaybe typeDef
    return $ Channel identl t

  
  typeDef = cspSym ":" >> typeExp
  typeExp = typeTuple <|> typeDot

  typeTuple = inSpan TypeTuple $
    between (cspSym "(") (cspSym ")")  (sepBy1Comma parseExp )

  typeDot = inSpan TypeDot $
    sepBy1 parseExpBase (cspSym ".")

  parsePrint :: PT LDecl
  parsePrint = withLoc $ do
    cspKey "print"
    e <- parseExp
    return $ Print e

procOpSharing :: PT (LProc -> LProc -> PT LProc)
procOpSharing = do
  spos <- getNextPos
  al <- between ( cspSym "[|" ) (cspSym "|]") parseExp
  epos <- getLastPos
  return $ (\a b  -> mkLabeledNode (mkSrcSpan spos epos) $ Fun3 "sharing" al a b)

closureExp :: PT LExp
closureExp = inSpan Closure $
  between (cspSym "{|") (cspSym "|}") 
          (sepBy1Comma parseExp )

{- Replicated Expressions in Prefix form -}

parseProcReplicatedExp :: PT LProc
parseProcReplicatedExp = do
      procRep ";"   ProcRepSequence
  <|> procRep "|~|" ProcRepInternalChoice
  <|> procRep "|||" ProcRepInterleave
  <|> procRep "[]"  ProcRepChoice
  <|> procRepAParallel
  <|> procRepLinkParallel
  <|> procRepSharing
  <|> parsePrefixExp
  <|> parseExpBase
  <?> "parseProcReplicatedExp"
  where
  -- todo : refactor all these to using inSpan
  procRep :: String -> ([LCompGen] -> LProc -> Exp) -> PT LProc
  procRep sym fkt = withLoc $ do
    cspSym sym
    l<-comprehensionRep
    body <- parseProcReplicatedExp
    return $ fkt l body

  procRepAParallel = withLoc $ do
    cspSym "||"
    l<-comprehensionRep
    cspSym "["
    alph <- parseExp
    cspSym "]"
    body <- parseProcReplicatedExp
    return $ ProcRepAParallel l alph body

  procRepLinkParallel = withLoc $ do
    link <- parseLinkList
    gen<-comprehensionRep
    body <- parseProcReplicatedExp
    return $ ProcRepLinkParallel gen link body

  procRepSharing = withLoc $ do
    al <- between ( cspSym "[|" ) (cspSym "|]") parseExp
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
    cspSym "->"
    return (channel,commfields)


{-
this is not what fdr really does
fdr parese ch?x.y:a as ch?((x.y):a)
-}
parseCommField :: PT LCommField
parseCommField = inComm <|> outComm <|> dotComm <?> "communication field"
  where
  inComm = withLoc $ do
    cspSym "?"
    updateState$ setLastChannelDir WasIn
    inCommCore

  inCommCore = do
    pat<-parsePatternNoDot
    guarD <- optionMaybe (cspSym ":" >> parseExp_noPrefix_NoDot)
    case guarD of
      Nothing -> return $ InComm pat
      Just g  -> return $ InCommGuarded pat g

  outComm = withLoc $ do
    cspSym "!" 
    updateState $ setLastChannelDir WasOut
    e <- parseExp_noPrefix_NoDot    
    return $ OutComm e

-- repeat the direction of the last CommField
  dotComm = withLoc $ do
    cspSym "."
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
  = newPos (sourceName pos) (-1) (unTokenId $ tokenId t)

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
   errorMsg = pprintParsecError err
  ,errorToken = errorTok
  ,errorPos = tokenStart errorTok
  }
  where 
    tokId = mkTokenId $ sourceColumn $ ParsecError.errorPos err
    errorTok = maybe tokenSentinel id  $ find (\t -> tokenId t ==  tokId) tl