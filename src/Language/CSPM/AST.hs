----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.AST
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This Module defines an Abstract Syntax Tree for CSPM.
-- This is the AST that is computed by the parser.
-- For historycal reasons, it is rather unstructured
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Language.CSPM.AST
where

import Language.CSPM.Token
import Language.CSPM.SrcLoc (SrcLoc(..))

import Data.Typeable (Typeable)
import Data.Generics.Basics (Data)
import Data.Generics.Instances ()
import Data.Ix
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Array.IArray

type AstAnnotation x = IntMap x
type Bindings = Map String UniqueIdent

newtype NodeId = NodeId {unNodeId :: Int}
  deriving (Show,Eq,Ord,Enum,Ix, Typeable, Data)

mkNodeId :: Int -> NodeId
mkNodeId = NodeId

data Labeled t = Labeled {
    nodeId :: NodeId
   ,unLabel :: t
   ,srcLoc  :: SrcLoc
   } deriving (Eq,Ord,Typeable, Data,Show)


-- | wrap a node with a dummyLabel
labeled :: t -> Labeled t
labeled t = Labeled {
  nodeId  = error "unknow nodeId"
 ,unLabel = t
 ,srcLoc  = NoLocation
 }

setNode :: Labeled t -> t -> Labeled t
setNode l n = l {unLabel = n}

type LIdent = Labeled Ident

data Ident 
  = Ident  {unIdent :: String}
  | UIdent {unUIdent :: UniqueIdent}
  deriving (Show,Eq,Ord,Typeable, Data)

identId :: LIdent -> Int
identId = uniqueIdentId . unUIdent . unLabel

data UniqueIdent = UniqueIdent
  {
   uniqueIdentId :: Int
  ,bindingSide :: NodeId
  ,bindingLoc  :: SrcLoc
  ,idType      :: IDType
  ,realName    :: String
  ,newName     :: String
  ,prologMode  :: PrologMode
  } deriving (Show,Eq,Ord,Typeable, Data)

data IDType 
  = VarID | ChannelID | NameTypeID | FunID Int
  | ConstrID String | DataTypeID | TransparentID
  deriving (Show,Eq,Ord,Typeable, Data)

data PrologMode = PrologGround | PrologVariable deriving (Show,Eq,Ord,Typeable, Data)

type LModule = Labeled Module
data Module = Module {
   moduleDecls :: [LDecl]
  ,moduleTokens :: Maybe [Token]
  } deriving (Show,Eq,Ord,Typeable, Data)


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
  | Fun1 LBuiltIn LExp
  | Fun2 LBuiltIn LExp LExp
  | DotTuple [LExp]
  | Closure [LExp]
  | ProcSharing LExp LProc LProc
  | ProcAParallel LExp LExp LExp LExp
  | ProcLinkParallel LLinkList LProc LProc
  | ProcRenaming [LRename] LProc
  | ProcRenamingComprehension [LRename] [LCompGen] LProc
  | ProcRepSequence LCompGenList LProc
  | ProcRepInternalChoice LCompGenList LProc
  | ProcRepInterleave LCompGenList LProc
  | ProcRepChoice LCompGenList LProc
  | ProcRepAParallel LCompGenList LExp LProc
  | ProcRepLinkParallel LCompGenList LLinkList LProc
  | ProcRepSharing LCompGenList LExp LProc
  | PrefixExp LExp [LCommField] LProc
  deriving (Show,Eq,Ord,Typeable, Data)

type LCompGenList = Labeled [LCompGen]

type LCommField = Labeled CommField
data CommField
  =  InComm LPattern
  | InCommGuarded LPattern LExp
  | OutComm LExp
  deriving (Show,Eq,Ord,Typeable, Data)

type LLinkList = Labeled LinkList
data LinkList
  = LinkList [LLink]
  | LinkListComprehension [LCompGen] [LLink]
  deriving (Show,Eq,Ord,Typeable, Data)

type LLink = Labeled Link
data Link = Link LExp LExp deriving (Show,Eq,Ord,Typeable, Data)

type LRename = Labeled Rename
data Rename = Rename LExp LExp deriving (Show,Eq,Ord,Typeable, Data)

type LBuiltIn = Labeled BuiltIn
data BuiltIn = BuiltIn Const deriving (Show,Eq,Ord,Typeable, Data)

lBuiltInToConst :: LBuiltIn -> Const
lBuiltInToConst = h . unLabel where
  h (BuiltIn c) = c

 --generators inside a comprehension-expression
type LCompGen = Labeled CompGen
data CompGen
  = Generator LPattern LExp
  | Guard LExp
  deriving (Show,Eq,Ord,Typeable, Data)


type LPattern = Labeled Pattern
data Pattern
  = IntPat Integer
  | TruePat
  | FalsePat
  | WildCard
{-  ConstrPat    -}
  | VarPat LIdent
  | Also [LPattern]
  | Append [LPattern]
  | DotPat [LPattern]
  | SingleSetPat LPattern
  | EmptySetPat
  | ListEnumPat [LPattern]
  | TuplePat [LPattern]
  | SelectorList {origPat :: LPattern , selectorList :: Array Int Selector , identList :: Array Int LIdent }
      -- this the result of pattern-match-compilation, inefficient, but simple
  deriving (Show,Eq,Ord,Typeable, Data)

{- a Selector is a path in a Pattern/Expression -}
data Selector
  = IntSel Integer
  | TrueSel
  | FalseSel
  | SelectThis
  | DotSel Int Int Selector
  | SingleSetSel Selector
  | EmptySetSel
  | ListLengthSel Int Selector
  | ListIthSel Int Selector
  | TupleLengthSel Int Selector
  | TupleIthSel Int Selector
  deriving (Show, Eq, Ord, Typeable, Data)

type LDecl = Labeled Decl
data Decl
  = PatBind LPattern LExp
  | FunBind LIdent [FunCase] -- todo : uses Labeled
  | AssertRef LExp String LExp
  | AssertBool LExp
  | Transparent [LIdent]
  | SubType LIdent [LConstructor]
  | DataType LIdent [LConstructor]
  | NameType LIdent LTypeDef
  | Channel [LIdent] (Maybe LTypeDef)
  | Print LExp
  deriving (Show,Eq,Ord,Typeable, Data)

type FunArgs = [[LPattern]] -- CSPM confusion of currying/tuples
data FunCase = FunCase FunArgs LExp   deriving (Show,Eq,Ord,Typeable, Data)

type LTypeDef = Labeled TypeDef
data TypeDef
  = TypeTuple [LExp]
  | TypeDot [LExp]
  deriving (Show,Eq,Ord,Typeable, Data)

type LConstructor = Labeled Constructor
data Constructor
  = Constructor LIdent (Maybe LTypeDef) 
  deriving (Show,Eq,Ord,Typeable, Data)



{-
some helper functions
-}
{- does not make sense if nodId should be unique -}
instance Functor Labeled where
  fmap f x = x {unLabel = f $ unLabel x }

withLabel :: ( NodeId -> a -> b ) -> Labeled a -> Labeled b
withLabel f x = x {unLabel = f (nodeId x) (unLabel x) }

mkLabeledNode :: (NodeIdSupply m) => SrcLoc -> t -> m (Labeled t)
mkLabeledNode loc node = do
  i <- getNewNodeId
  return $ Labeled {
    nodeId = i
   ,srcLoc = loc
   ,unLabel = node }

{-
-- user must supply a unique NodeId
unsafeMkLabeledNode :: NodeId -> SrcLoc -> t -> Labeled t
unsafeMkLabeledNode i loc node
  = Labeled {
    nodeId = i
   ,srcLoc = loc
   ,unLabel = node }
-}

class (Monad m) => NodeIdSupply m where
  getNewNodeId :: m NodeId


data Const
  = F_true
  | F_false
  | F_not
  | F_and
  | F_or
  | F_union
  | F_inter
  | F_diff
  | F_Union
  | F_Inter
  | F_member
  | F_card
  | F_empty
  | F_set
  | F_Set
  | F_Seq
  | F_null
  | F_head
  | F_tail
  | F_concat
  | F_elem
  | F_length
  | F_STOP
  | F_SKIP
  | F_Events
  | F_Int
  | F_Bool
  | F_CHAOS
  | F_Concat
  | F_Len2
  | F_Mult
  | F_Div
  | F_Mod
  | F_Add
  | F_Sub
  | F_Eq
  | F_NEq
  | F_GE
  | F_LE
  | F_LT
  | F_GT
  | F_Guard
  | F_Sequential
  | F_Interrupt
  | F_ExtChoice
  | F_Timeout
  | F_IntChoice
  | F_Interleave
  | F_Hiding
  deriving (Show,Eq,Ord,Typeable, Data)
