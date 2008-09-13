{-
todo : check that we do not bind variables when we patternmacht against
constructors : add a testcase for that
todo :: maybe use SYB for gathering the renaming
todo :: maybe also compute debruin-index/ freevariables
todo :: check idType in useIdent
fix topleveldecls to toplevel ? -> allready done by parser
fix subtype bug

* Process all data-type and channel decls before all value bindings
-}
module Language.CSPM.Rename
  (
   getRenaming
  ,applyRenaming
  ,RenameError(..)
  )
where

import Language.CSPM.AST hiding (prologMode)
import qualified Language.CSPM.AST as AST

import qualified Data.Generics.Schemes (everywhere)
import qualified Data.Generics.Aliases (mkT)
import Data.Typeable (Typeable)

import Control.Monad.Error
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

getRenaming ::
     LModule 
  -> Either RenameError (AstAnnotation UniqueIdent,AstAnnotation UniqueIdent)
getRenaming m
  = case execStateT (rnModule m) initialRState of
      Right state -> Right (identDefinition state, identUse state)
      Left e -> Left e

type RM x = StateT RState (Either RenameError) x

type Bindings = Map String UniqueIdent
type UniqueName = Int

data RState = RState
  {
    nameSupply :: UniqueName
-- used to check that we do not bind a name twice inside a pattern
   ,localBindings :: Bindings  
   ,visible  :: Bindings       -- everything that is visible
   ,identDefinition :: AstAnnotation UniqueIdent 
   ,identUse  :: AstAnnotation UniqueIdent 
   ,usedNames :: Set String
   ,prologMode :: PrologMode
  } deriving Show

initialRState :: RState
initialRState = RState
  {
    nameSupply    = 0
   ,localBindings = Map.empty
   ,visible       = Map.empty
   ,identDefinition = IntMap.empty
   ,identUse        = IntMap.empty
   ,usedNames       = Set.empty
   ,prologMode      = PrologVariable
  }

data RenameError
  = RenameError {
   errorMsg :: String
  ,errorLoc :: SrcLoc
  } deriving (Show,Typeable)

instance Error RenameError where
  noMsg = RenameError { errorMsg = "no Messsage", errorLoc = NoLocation }
  strMsg m = RenameError { errorMsg = m, errorLoc = NoLocation }

bindNewTopIdent :: IDType -> LIdent -> RM ()
bindNewTopIdent t i = do
  let (Ident origName) = unLabel i
  vis <- gets visible
  case Map.lookup origName vis of
    Nothing -> bindNewUniqueIdent t i
    Just _ -> throwError $ RenameError {
      errorMsg = "Redefinition of toplevel name " ++ origName
     ,errorLoc = srcLoc i }

bindNewUniqueIdent :: IDType -> LIdent -> RM ()
bindNewUniqueIdent iType lIdent = do
  let (Ident origName) = unLabel lIdent
  local <- gets localBindings
  {- check that we do not bind a variable twice i.e. in a pattern -}
  case Map.lookup origName local of
    Nothing -> return ()
    Just _ -> throwError $ RenameError {
       errorMsg = "Redefinition of " ++ origName
       ,errorLoc = srcLoc lIdent }
{- 
  If we have a Constructor in scope and try to bind
  a VarID then we actually have a Constructor-Pattern.
  Same situation for a channelIDs.
  We throw an error if the csp-code tries to reuse a constructor
  or a channel for i.e. a function.
-}
  vis <- gets visible
  case (Map.lookup origName vis,iType) of
   (Just x ,VarID) -> case idType x of
     ConstrID _ -> useExistingBinding x
     ChannelID -> useExistingBinding x
     _ -> addNewBinding
   (Just x , _) -> case idType x of
     ConstrID _-> throwError $ RenameError {
          errorMsg = "Illigal reuse of Contructor " ++ origName
         ,errorLoc = srcLoc lIdent }
     ChannelID -> throwError $ RenameError {
          errorMsg = "Illigal reuse of Channel " ++ origName
         ,errorLoc = srcLoc lIdent }
     _ -> addNewBinding
   (_, _ )  -> addNewBinding
  where
    useExistingBinding :: UniqueIdent -> RM ()
    useExistingBinding constr = do
      let ptr = unNodeId $ nodeId $ lIdent
      modify $ \s -> s
        { identDefinition = IntMap.insert ptr constr (identDefinition s) }

    addNewBinding :: RM ()
    addNewBinding = do
      let (Ident origName) = unLabel lIdent
          nodeID = nodeId lIdent
    
      (nameNew,unique) <- nextUniqueName origName
      plMode <- gets prologMode
      let uIdent = UniqueIdent {
         uniqueIdentId = unique
        ,bindingSide = nodeID
        ,bindingLoc  = srcLoc lIdent
        ,idType = iType
        ,realName = origName
        ,newName = nameNew
        ,AST.prologMode = plMode }
      modify $ \s -> s 
        { localBindings = Map.insert origName uIdent $ localBindings s
        , visible       = Map.insert origName uIdent $ visible s }
      modify $ \s -> s
        { identDefinition = IntMap.insert 
            (unNodeId nodeID) uIdent $ identDefinition s }
      return ()

    nextUniqueName :: String -> RM (String,UniqueName)
    nextUniqueName oldName = do
      n <- gets nameSupply
      modify $ \s -> s {nameSupply = succ n}
      occupied <- gets usedNames
      let
         suffixes = "" : map show ([2..9] ++ [n + 10 .. ])
         candidates = map ((++) oldName) suffixes
         nextName = head $ filter (\x -> not $ Set.member x occupied) candidates
      modify $ \s -> s {usedNames = Set.insert nextName $ usedNames s}
      return (nextName,n)

localScope :: RM x -> RM x
localScope h = do 
  vis <- gets visible
  localBind <- gets localBindings
  modify $ \s -> s {localBindings = Map.empty}
  res <- h                 
  modify $ \e -> e {
     visible = vis
    ,localBindings = localBind }
  return res


{-
rn just walks through the AST, without modifing it.
The actual renamings are stored in a sepearte AstAnnotation inside the RM-Monad
-}



nop :: RM ()
nop = return ()

rnModule :: LModule -> RM ()
rnModule m = rnDeclList $ moduleDecls $ unLabel m

rnExpList :: [LExp] -> RM ()
rnExpList = mapM_ rnExp

-- rename an expression
rnExp :: LExp -> RM ()
rnExp expression = case unLabel expression of
  Var ident -> useVarIdent ident
  IntExp _ -> nop
  SetEnum a -> rnExpList a
  ListEnum a -> rnExpList a
  SetOpen a -> rnExp a
  ListOpen a -> rnExp a
  SetClose (a,b) -> rnExp a >> rnExp b
  ListClose (a,b) -> rnExp a >> rnExp b
  SetComprehension (a,b) -> inCompGen b (rnExpList a)
  ListComprehension (a,b) -> inCompGen b (rnExpList a)
  ClosureComprehension (a,b) -> inCompGen b (rnExpList a)
  Let decls e -> localScope (rnDeclList decls >> rnExp e)
  Ifte a b c -> rnExp a >> rnExp b >> rnExp c
  CallFunction a args -> rnExp a >> mapM_ rnExpList args
  CallBuiltIn _ args -> mapM_ rnExpList args
  Lambda pList e -> localScope (rnPatList pList >> rnExp e)
  Stop -> nop
  Skip -> nop
  CTrue -> nop
  CFalse -> nop
  Events -> nop
  BoolSet -> nop
  IntSet -> nop
  TupleExp l -> rnExpList l
  Parens a -> rnExp a 
  AndExp a b -> rnExp a >> rnExp b
  OrExp a b -> rnExp a >> rnExp b
  NotExp a -> rnExp a
  Fun1 _ a -> rnExp a
  Fun2 _ a b -> rnExp a >> rnExp b
  DotTuple l -> rnExpList l
  Closure l -> rnExpList l
  ProcSharing al p1 p2 -> rnExp al >> rnExp p1 >> rnExp p2
  ProcAParallel a b c d -> rnExp a >> rnExp b >> rnExp c >> rnExp d
  ProcLinkParallel l e1 e2 -> rnLinkList l >> rnExp e1 >> rnExp e2
  ProcRenaming rlist e -> mapM_ reRename rlist >> rnExp e
{- scopingrules for LCompGen as found in FDR -} 
  ProcRenamingComprehension re comp proc
     -> inCompGen comp (mapM_ reRename re) >> rnExp proc
  ProcRepSequence a p -> inCompGen a (rnExp p)
  ProcRepInternalChoice a p -> inCompGen a (rnExp p)
  ProcRepInterleave a p -> inCompGen a (rnExp p)
  ProcRepChoice  a p -> inCompGen a (rnExp p)
  ProcRepAParallel comp a p -> inCompGen comp (rnExp a >> rnExp p)
  ProcRepLinkParallel comp l p
    -> rnLinkList l >> inCompGen comp (rnExp p)
  ProcRepSharing comp s p -> rnExp s >> inCompGen comp (rnExp p)
  PrefixExp chan fields proc -> localScope $ do
    rnExp chan
    mapM_ rnCommField fields
    rnExp proc
  where 
    {- 
    called from VarExp
    we can bind lIdent to any Identifier that is in scope
    (ConstID,FunID ..)
    -}
    useVarIdent :: LIdent -> RM ()
    useVarIdent lIdent = do
      let (Ident origName) = unLabel lIdent
          nodeID = nodeId lIdent
      vis <- gets visible
      case Map.lookup origName vis of
        Nothing -> throwError $ RenameError {
           errorMsg = "Unbound Identifier :" ++ origName
           ,errorLoc = srcLoc lIdent }
        Just uniqueIdent -> do   -- todo check idType
           modify $ \s -> s
             { identUse =  IntMap.insert 
                 (unNodeId nodeID) uniqueIdent $ identUse s }
           return ()


rnPatList :: [LPattern] -> RM ()
rnPatList = mapM_ rnPattern

rnPattern :: LPattern -> RM ()
rnPattern p = case unLabel p of
  IntPat _ -> nop
  TruePat -> nop
  FalsePat -> nop
  WildCard -> nop
  VarPat lIdent -> bindNewUniqueIdent VarID lIdent
  Also l -> rnPatList l
  Append l -> rnPatList l
  DotPat l -> rnPatList l
  SingleSetPat a -> rnPattern a
  EmptySetPat -> nop
  ListEnumPat l -> rnPatList l
  TuplePat l -> rnPatList l

rnCommField :: LCommField -> RM ()
rnCommField f = case unLabel f of
  InComm pat -> rnPattern pat
  InCommGuarded p g -> rnPattern p >> rnExp g
  OutComm e -> rnExp e

inCompGen :: [LCompGen] -> RM () -> RM ()
inCompGen (h:t) ret = localScope $ do
  rnCompGen h
  inCompGen t ret
inCompGen [] ret = ret 

rnCompGen :: LCompGen -> RM ()
rnCompGen g = case unLabel g of
  Generator pat e -> rnExp e >> rnPattern pat
  Guard e -> rnExp e

rnTypeDef :: LTypeDef -> RM ()
rnTypeDef t = case unLabel t of
  TypeTuple l -> rnExpList l
  TypeDot l -> rnExpList l

rnConstructor :: LConstructor -> RM ()
rnConstructor = rc . unLabel where
  rc (Constructor c td) = do
    bindNewTopIdent (ConstrID "someConstructor") c --Todo -- fix
    case td of
      Nothing -> nop
      Just t  -> rnTypeDef t

reRename :: LRename -> RM ()
reRename = r2 . unLabel
  where r2 (Rename e1 e2) = rnExp e1 >> rnExp e2

rnLinkList :: LLinkList -> RM ()
rnLinkList = rnLink2 . unLabel
  where 
    rnLink2 (LinkList l) = mapM_ rnLink l
    rnLink2 (LinkListComprehension a b) = inCompGen a (mapM_ rnLink b)

    rnLink = (\(Link a b) ->rnExp a >> rnExp b) . unLabel


-- rename a recursive binding group
rnDeclList :: [LDecl] -> RM ()
rnDeclList declList = mapM_ declLHS declList >> mapM_ declRHS declList

declLHS :: LDecl -> RM ()
declLHS d = prologGroundMode $ case unLabel d of
  PatBind pat _ -> rnPattern pat
   --todo : proper type-checking/counting number of Funargs
  FunBind i _ -> bindNewUniqueIdent (FunID (-1)) i
  AssertRef {} -> nop
  AssertBool {} -> nop
  Transparent tl -> mapM_ (bindNewTopIdent TransparentID) tl
  SubType i _ -> bindNewTopIdent DataTypeID i   -- fix this
  DataType i _ -> bindNewTopIdent DataTypeID i
  NameType i _ -> bindNewTopIdent NameTypeID i
  Channel chList _ -> mapM_ (bindNewTopIdent ChannelID) chList
  Print _ -> nop
  where
    prologGroundMode action = do
      modify $ \s -> s {prologMode = PrologGround }
      action
      modify $ \s -> s {prologMode = PrologVariable }

declRHS :: LDecl -> RM ()
declRHS d = case unLabel d of
  PatBind _ e -> rnExp e
  FunBind _ cases -> mapM_ rnFunCase cases
  AssertRef a _ b -> rnExp a >> rnExp b
  AssertBool e -> rnExp e
  Transparent _ -> nop  
  SubType _ clist -> localScope $ mapM_ rnConstructor clist 
  DataType _ clist -> mapM_ rnConstructor clist
  NameType _ td -> rnTypeDef td
  Channel _ Nothing -> nop
  Channel _ (Just td) -> rnTypeDef td
  Print e -> rnExp e
  where
    rnFunCase c = case c of  --todo:uses Labeled version
      (FunCase pat e) -> localScope (mapM_ rnPatList pat >> rnExp e)

applyRenaming ::
     (AstAnnotation UniqueIdent,AstAnnotation UniqueIdent)
  -> LModule 
  -> LModule
applyRenaming (defIdent,usedIdent) ast
  = Data.Generics.Schemes.everywhere (Data.Generics.Aliases.mkT patchIdent) ast
  where
    patchIdent :: LIdent -> LIdent
    patchIdent l =
      let nodeID = unNodeId $ nodeId l in
      case IntMap.lookup nodeID usedIdent of
        Just u -> l { unLabel = UIdent u}
        Nothing -> case IntMap.lookup nodeID defIdent of
          Just d -> l { unLabel = UIdent d}
          Nothing -> error $ "internal error: patchIdent nodeId :" ++ show nodeID