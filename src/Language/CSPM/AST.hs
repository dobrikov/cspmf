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
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Array.IArray

type AstAnnotation x = IntMap x
type Bindings = Map String UniqueIdent
type FreeNames = IntMap UniqueIdent

newtype NodeId = NodeId {unNodeId :: Int}
  deriving ( Eq, Ord, Show,Enum,Ix, Typeable, Data)

mkNodeId :: Int -> NodeId
mkNodeId = NodeId

data Labeled t = Labeled {
    nodeId :: NodeId
   ,srcLoc  :: SrcLoc
   ,unLabel :: t
   } deriving ( Eq, Ord, Typeable, Data,Show)

-- | Wrap a node with a dummyLabel
-- | todo: redo we need a specal case in DataConstructor Labeled
-- | also rename to unLabeled
labeled :: t -> Labeled t
labeled t = Labeled {
  --error use hashfunction here 
 nodeId  = NodeId (-1)   --error "unknown nodeId"
 ,unLabel = t
 ,srcLoc  = NoLocation
 }

setNode :: Labeled t -> y -> Labeled y
setNode l n = l {unLabel = n}

type LIdent = Labeled Ident

data Ident 
  = Ident  {unIdent :: String}
  | UIdent {unUIdent :: UniqueIdent}
  deriving ( Eq, Ord, Show,Typeable, Data)

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
  ,bindType    :: BindType
  } deriving ( Eq, Ord, Show,Typeable, Data)

data IDType 
  = VarID | ChannelID | NameTypeID | FunID Int
  | ConstrID String | DataTypeID | TransparentID
  deriving ( Eq, Ord, Show,Typeable, Data)

{- actually BindType and PrologMode are semantically aquivalent -}
data BindType = LetBound | NotLetBound
  deriving ( Eq, Ord, Show,Typeable, Data)

data PrologMode = PrologGround | PrologVariable
  deriving ( Eq, Ord, Show,Typeable, Data)

type LModule = Labeled Module
data Module = Module {
   moduleDecls :: [LDecl]
  ,moduleTokens :: Maybe [Token]
  } deriving ( Eq, Ord, Show,Typeable, Data)


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
  | SetExp LRange (Maybe [LCompGen])
  | ListExp LRange (Maybe [LCompGen])
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
  | ProcSet
  | TupleExp [LExp]
  | Parens LExp
  | AndExp LExp LExp
  | OrExp LExp LExp
  | NotExp LExp
  | NegExp LExp
  | Fun1 LBuiltIn LExp
  | Fun2 LBuiltIn LExp LExp
  | DotTuple [LExp]
  | Closure [LExp]
  | ProcSharing LExp LProc LProc
  | ProcAParallel LExp LExp LProc LProc
  | ProcLinkParallel LLinkList LProc LProc
  | ProcRenaming [LRename] (Maybe LCompGenList) LProc
  | ProcException LProc LExp LProc
  | ProcRepSequence LCompGenList LProc
  | ProcRepInternalChoice LCompGenList LProc
  | ProcRepExternalChoice LCompGenList LProc
  | ProcRepInterleave LCompGenList LProc
  | ProcRepAParallel LCompGenList LExp LProc
  | ProcRepLinkParallel LCompGenList LLinkList LProc
  | ProcRepSharing LCompGenList LExp LProc--
  | PrefixExp LExp [LCommField] LProc--
-- only used in later stages
  | PrefixI FreeNames LExp [LCommField] LProc
  | LetI [LDecl] FreeNames LExp -- freenames of all localBound names
  | LambdaI FreeNames [LPattern] LExp
  | ExprWithFreeNames FreeNames LExp
  deriving ( Eq, Ord, Show, Typeable, Data)

type LRange = Labeled Range
data Range
  = RangeEnum [LExp]
  | RangeClosed LExp LExp
  | RangeOpen LExp
  deriving ( Eq, Ord, Show, Typeable, Data)

type LCommField = Labeled CommField
data CommField
  =  InComm LPattern
  | InCommGuarded LPattern LExp
  | OutComm LExp
  deriving ( Eq, Ord, Show,Typeable, Data)

type LLinkList = Labeled LinkList
data LinkList
  = LinkList [LLink]
  | LinkListComprehension [LCompGen] [LLink]
  deriving ( Eq, Ord, Show,Typeable, Data)

type LLink = Labeled Link
data Link = Link LExp LExp deriving ( Eq, Ord, Show,Typeable, Data)

type LRename = Labeled Rename
data Rename = Rename LExp LExp deriving ( Eq, Ord, Show,Typeable, Data)

type LBuiltIn = Labeled BuiltIn
data BuiltIn = BuiltIn Const deriving ( Eq, Ord, Show,Typeable, Data)

lBuiltInToConst :: LBuiltIn -> Const
lBuiltInToConst = h . unLabel where
  h (BuiltIn c) = c

type LCompGenList = Labeled [LCompGen]
type LCompGen = Labeled CompGen
data CompGen
  = Generator LPattern LExp
  | Guard LExp
  deriving ( Eq, Ord, Show,Typeable, Data)

type LPattern = Labeled Pattern
data Pattern
  = IntPat Integer
  | TruePat
  | FalsePat
  | WildCard
  | ConstrPat LIdent
  | Also [LPattern]
  | Append [LPattern]
  | DotPat [LPattern]
  | SingleSetPat LPattern
  | EmptySetPat
  | ListEnumPat [LPattern]
  | TuplePat [LPattern]
-- this the result of pattern-match-compilation
  | VarPat LIdent
  | Selectors { --origPat :: LPattern
 -- fixme this creates an infinite tree with SYB everywehre'
                selectors :: Array Int Selector
               ,idents :: Array Int (Maybe LIdent) }
  | Selector Selector (Maybe LIdent)
  deriving ( Eq, Ord, Show,Typeable, Data)

{- a Selector is a path in a Pattern/Expression -}
data Selector
  = IntSel Integer
  | TrueSel
  | FalseSel
  | SelectThis
  | ConstrSel UniqueIdent  
  | DotSel Int Selector
  | SingleSetSel Selector
  | EmptySetSel
  | TupleLengthSel Int Selector
  | TupleIthSel Int Selector
  | ListLengthSel Int Selector
  | ListIthSel Int Selector
  | HeadSel Selector
  | HeadNSel Int Selector
  | PrefixSel Int Int Selector
  | TailSel Selector
  | SliceSel Int Int Selector
  | SuffixSel Int Int Selector
  deriving ( Eq, Ord, Show, Typeable, Data)

type LDecl = Labeled Decl
data Decl
  = PatBind LPattern LExp
  | FunBind LIdent [FunCase]
  | Assert LAssertDecl
  | Transparent [LIdent]
  | SubType LIdent [LConstructor]
  | DataType LIdent [LConstructor]
  | NameType LIdent LTypeDef
  | Channel [LIdent] (Maybe LTypeDef)
  | Print LExp
  deriving (Show,Eq,Ord,Typeable, Data)

{-
We want to use                1) type FunArgs = [LPattern]
it is not clear why we used   2) type FunArgs = [[LPattern]]

If 1) works in the interpreter, we will refactor
Renaming , and prolog-interface to 1)

For now we just patch the AST Just before PatternCompilation
-}
type FunArgs = [[LPattern]] -- CSPM confusion of currying/tuples
data FunCase 
  = FunCase FunArgs LExp       -- oldVersion (returned by parser)
  | FunCaseI [LPattern] LExp   -- newVersion for interpreter
  deriving ( Eq, Ord, Show,Typeable, Data)

type LTypeDef = Labeled TypeDef
data TypeDef
  = TypeTuple [LExp]
  | TypeDot [LExp]
  deriving ( Eq, Ord, Show,Typeable, Data)

type LConstructor = Labeled Constructor
data Constructor
  = Constructor LIdent (Maybe LTypeDef) 
  deriving ( Eq, Ord, Show,Typeable, Data)

withLabel :: ( NodeId -> a -> b ) -> Labeled a -> Labeled b
withLabel f x = x {unLabel = f (nodeId x) (unLabel x) }

type LAssertDecl = Labeled AssertDecl
data AssertDecl
  = AssertBool LExp
  | AssertRefine LExp LRefineOp LExp
  | AssertTauPrio LExp LTauRefineOp LExp LExp
  | AssertModelCheck LExp LFDRModels
  deriving ( Eq, Ord, Show,Typeable,Data)

type LFDRModels = Labeled FDRModels
data FDRModels
  = DeadlockFreeF
  | DeadlockFreeFD
  | DeterministicF
  | DeterministicFD
  | LivelockFree
  deriving ( Eq, Ord, Show,Typeable,Data)

type LTauRefineOp = Labeled TauRefineOp 
data TauRefineOp
  = TauTrace
  | TauRefine
 deriving ( Eq, Ord, Show,Typeable, Data)

type LRefineOp = Labeled RefineOp
data RefineOp 
  = Trace
  | Failure
  | FailureDivergence
  | RefusalTesting
  | RefusalTestingDiv
  | RevivalTesting
  | RevivalTestingDiv
  | TauPriorityOp
  deriving ( Eq, Ord, Show,Typeable, Data)

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
  | F_concat -- fix confusing F_Concat
  | F_elem
  | F_length
  | F_STOP
  | F_SKIP
  | F_Events
  | F_Int
  | F_Bool
  | F_CHAOS
  | F_Concat -- fix confusing F_concat
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
  | F_IntChoice
  | F_Hiding
  | F_Timeout
  | F_Interleave
  deriving ( Eq, Ord, Show,Typeable, Data)
