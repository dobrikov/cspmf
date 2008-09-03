-- todo : use a datatype for build-ins / operators
-- remove all "unparsed" strings

{-# LANGUAGE DeriveDataTypeable #-}
module Language.CSPM.AST
where

import Data.Typeable

type TokenId = Int
data SrcLoc
  = TokPos TokenId
  | TokSpan TokenId TokenId
  | NoLocation
  deriving (Show,Eq,Ord,Typeable)

type Label = SrcLoc

data Labeled t = Labeled {label :: Label, unLabel :: t}
  deriving (Show,Eq,Ord,Typeable)

instance Functor Labeled where
  fmap f x = Labeled (label x) (f $ unLabel x)
 
withLabel :: ( Label -> a -> b ) -> Labeled a -> Labeled b
withLabel f x = Labeled (label x) (f (label x) (unLabel x))

withLabelM :: 
  Monad m => ( Label -> a -> m b ) -> Labeled a -> m (Labeled b)
withLabelM f x = do
  let l = label x
  r <- f l $ unLabel x
  return $ Labeled l r 

labeled :: Label -> t -> Labeled t
labeled = Labeled

data Module = Module [LDecl]
  deriving (Show,Eq,Ord,Typeable)

type LIdent = Labeled Ident

data Ident 
  = Ident  {unIdent :: String}
  | UIdent UniqueIdent
  deriving (Show,Eq,Ord,Typeable)

data UniqueIdent = UniqueIdent
  {
   uniqueIdentId :: Int
  ,bindingSide :: Label
  ,idType      :: IDType
  ,realName    :: String
  } deriving (Show,Eq,Ord,Typeable)

data IDType 
  = VarID | ChannelID | NameTypeID | FunID Int | IdentID
  | ConstrID String | DataTypeID | TransparentID
  deriving (Show,Eq,Ord,Typeable)

{-
LProc is just a typealias for better readablility
todo : maybe use a real type
-}
type LProc = LExp

-- expressions
type LExp = Labeled Exp

data Exp
  = Var LIdent
  | IntExp Integer
  | SetEnum [LExp]
  | ListEnum [LExp]
  | SetOpen LExp
  | ListOpen LExp
  | SetClose (LExp,LExp)
  | ListClose (LExp,LExp)
  | SetComprehension ([LExp],[LCompGen])
  | ListComprehension ([LExp],[LCompGen])
  | ClosureComprehension ([LExp],[LCompGen])
  | Let [LDecl] LExp
  | Ifte LExp LExp LExp
  | CallFunction LExp [[LExp]]
  | CallBuiltIn LBuiltIn [[LExp]]
  | Lambda [LPattern] LExp
  | Stop
  | Skip
  | CTrue
  | CFalse
  | Events
  | BoolSet
  | IntSet
  | TupleExp [LExp]
  | Parens LExp
  | AndExp LExp LExp
  | OrExp LExp LExp
  | NotExp LExp
  | Fun1 String LExp    -- remove String
  | Fun2 String LExp LExp  -- remove String
  | Fun3 String LExp LExp LExp  -- remove String
  | DotTuple [LExp]
  | Closure [LExp]
  | ProcAParallel LExp LExp LExp LExp
  | ProcLinkParallel LLinkList LProc LProc
  | ProcRenaming [LRename] LProc
  | ProcRenamingComprehension [LRename] [LCompGen] LProc
  | ProcRepSequence [LCompGen] LProc
  | ProcRepInternalChoice [LCompGen] LProc
  | ProcRepInterleave [LCompGen] LProc
  | ProcRepChoice [LCompGen] LProc
  | ProcRepAParallel [LCompGen] LExp LProc
  | ProcRepLinkParallel [LCompGen] LLinkList LProc
  | ProcRepSharing [LCompGen] LExp LProc
  | PrefixExp LExp [LCommField] LProc
  deriving (Show,Eq,Ord,Typeable)


type LCommField = Labeled CommField
data CommField
  =  InComm LPattern
  | InCommGuarded LPattern LExp
  | OutComm LExp
  deriving (Show,Eq,Ord,Typeable)

type LLinkList = Labeled LinkList
data LinkList
  = LinkList [LLink]
  | LinkListComprehension [LCompGen] [LLink]
  deriving (Show,Eq,Ord,Typeable)

type LLink = Labeled Link
data Link = Link LExp LExp deriving (Show,Eq,Ord,Typeable)

type LRename = Labeled Rename
data Rename = Rename LExp LExp deriving (Show,Eq,Ord,Typeable)

type LBuiltIn = Labeled BuiltIn
data BuiltIn = BuiltIn String deriving (Show,Eq,Ord,Typeable)

 --generators inside a comprehension-expression
type LCompGen = Labeled CompGen
data CompGen
  = Generator LPattern LExp
  | Guard LExp
  deriving (Show,Eq,Ord,Typeable)


type LPattern = Labeled Pattern
data Pattern
  = IntPat Integer
  | TruePat
  | FalsePat
  | WildCard
  | VarPat LIdent
  | Also [LPattern]
  | Append [LPattern]
  | DotPat [LPattern]
  | SingleSetPat LPattern
  | EmptySetPat
  | ListEnumPat [LPattern]
  | TuplePat [LPattern]
  deriving (Show,Eq,Ord,Typeable)

type LDecl = Labeled Decl
data Decl
  = PatBind LPattern LExp
  | FunBind LIdent [FunCase]
  | AssertRef LExp String LExp
  | AssertBool LExp
  | Transparent [LIdent]
  | SubType LIdent [LConstructor]
  | DataType LIdent [LConstructor]
  | NameType LIdent LTypeDef
  | Channel [LIdent] (Maybe LTypeDef)
  | Print LExp
  deriving (Show,Eq,Ord,Typeable)

type FunArgs = [[LPattern]] -- CSPM confusion of currying/tuples
data FunCase = FunCase FunArgs LExp   deriving (Show,Eq,Ord,Typeable)

type LTypeDef = Labeled TypeDef
data TypeDef
  = TypeTuple [LExp]
  | TypeDot [LExp]
  deriving (Show,Eq,Ord,Typeable)

type LConstructor = Labeled Constructor
data Constructor
  = Constructor LIdent (Maybe LTypeDef) 
  deriving (Show,Eq,Ord,Typeable)
