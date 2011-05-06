-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Rename
-- Copyright   :  (c) Fontaine 2008 - 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Compute the mapping between the using occurences and the defining occurences of all Identifier in a Module
-- Also decide whether to use ground or non-ground- representaions for the translation to Prolog.

{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Language.CSPM.Rename
  (
   renameModule
  ,RenameError (..)
  ,RenameInfo (..)
  ,ModuleFromRenaming
  ,FromRenaming
  )
where

import Language.CSPM.AST hiding (prologMode, bindType)
import qualified Language.CSPM.AST as AST
import qualified Language.CSPM.SrcLoc as SrcLoc
import Language.CSPM.BuiltIn as BuiltIn

import Data.Generics.Basics (Data(..))
import Data.Data (mkDataType)
import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

import Control.Monad.Error
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.List as List
import Data.Maybe

instance Data FromRenaming
  where
    gunfold = error "instance Data FromRenaming gunfold"
    toConstr = error "instance Data FromRenaming toConstr"
    dataTypeOf _ = mkDataType "Language.CSPM.Rename.FromRenaming" []

-- | A module that has gone through renaming
type ModuleFromRenaming = Module FromRenaming

-- | Tag that a module has gone through renaming.
data FromRenaming deriving Typeable

-- | 'renameModule' renames a 'Module'.
-- | (also calls mergeFunBinds)
renameModule ::
     ModuleFromParser
  -> Either RenameError (ModuleFromRenaming, RenameInfo)
renameModule m = do
  let m' = mergeFunBinds m
  st <- execStateT (initPrelude >> rnModule m') initialRState
  return
    (
     applyRenaming m' (identDefinition st) (identUse st)
    ,st)

type RM x = StateT RenameInfo (Either RenameError) x

type UniqueName = Int

-- | Gather all information about an renaming. 
data RenameInfo = RenameInfo
  {
    nameSupply :: Int
   ,localBindings :: Map String UniqueIdent -- used to check that we do not bind a name twice inside a pattern
   ,visible  :: Map String UniqueIdent      -- everything that is visible
   ,identDefinition :: AstAnnotation UniqueIdent
   ,identUse  :: AstAnnotation UniqueIdent
   ,usedNames :: Set String
   ,prologMode :: PrologMode -- could use a readermonad for prologMode and bindType
   ,bindType   :: BindType
  } deriving Show

initialRState :: RenameInfo
initialRState = RenameInfo {..}
  where
    nameSupply    = 0
    localBindings = Map.empty
    visible       = Map.empty
    identDefinition = IntMap.empty
    identUse        = IntMap.empty
    usedNames       = Set.empty
    prologMode      = PrologVariable
    bindType        = NotLetBound

initPrelude :: RM ()
initPrelude
    = forM_ BuiltIn.builtIns $ \bi -> do
        bindNewTopIdent BuiltInID (labeled $ Ident bi)

data RenameError
  = RenameError {
   renameErrorMsg :: String
  ,renameErrorLoc :: SrcLoc.SrcLoc
  } deriving (Show,Typeable)

instance Exception RenameError


instance Error RenameError where
  noMsg = RenameError { renameErrorMsg = "no Messsage", renameErrorLoc = SrcLoc.NoLocation }
  strMsg m = RenameError { renameErrorMsg = m, renameErrorLoc = SrcLoc.NoLocation }

lookupVisible :: LIdent -> RM (Maybe UniqueIdent)
lookupVisible i = do
  vis <- gets visible
  return $ Map.lookup (unIdent $ unLabel i) vis

getOrigName :: LIdent -> String
getOrigName = unIdent . unLabel

bindNewTopIdent :: IDType -> LIdent -> RM ()
bindNewTopIdent t i = do
  vis <- lookupVisible i
  case vis of
    Nothing -> bindNewUniqueIdent t i
    Just _ -> throwError $ RenameError {
      renameErrorMsg = "Redefinition of toplevel name " ++ getOrigName i
     ,renameErrorLoc = srcLoc i }

bindNewUniqueIdent :: IDType -> LIdent -> RM ()
bindNewUniqueIdent iType lIdent = do
  let origName = getOrigName lIdent
  {- check that we do not bind a variable twice i.e. in a pattern -}
  local <- gets localBindings
  when (isJust $ Map.lookup origName local) $
    throwError $ RenameError {
       renameErrorMsg = "Redefinition of " ++ origName
       ,renameErrorLoc = srcLoc lIdent }
  vis <- lookupVisible lIdent
  case vis of
   Nothing -> addNewBinding
   (Just u) -> case (iType, idType u) of
      {- If there is a Constructor of Channel in scope and we try to bind a VarID
      this VarID is a pattern match for the existing binding -}

      (VarID, ConstrID) -> useExistingBinding u
      (VarID, ChannelID) -> useExistingBinding u

      (VarID, _) -> addNewBinding
      {- We throw an error if the csp-code tries to rebind a constructor or a channel ID -}
      (_    , ConstrID) -> throwError $ RenameError {
          renameErrorMsg = "Illigal reuse of Contructor " ++ origName
         ,renameErrorLoc = srcLoc lIdent }
      (_    , ChannelID) -> throwError $ RenameError {
          renameErrorMsg = "Illigal reuse of Channel " ++ origName
         ,renameErrorLoc = srcLoc lIdent }

      (_, _) -> addNewBinding
  where
    useExistingBinding :: UniqueIdent -> RM ()
    useExistingBinding ident = do
      let ptr = unNodeId $ nodeId $ lIdent
      modify $ \s -> s
        { identDefinition = IntMap.insert ptr ident $ identDefinition s }

    addNewBinding :: RM ()
    addNewBinding = do
      let origName = unIdent $ unLabel lIdent
          nodeID = nodeId lIdent
    
      (nameNew,unique) <- nextUniqueName origName
      plMode <- gets prologMode
      bType  <- gets bindType
      let uIdent = UniqueIdent {
         uniqueIdentId = unique
        ,bindingSide = nodeID
        ,bindingLoc  = srcLoc lIdent
        ,idType = iType
        ,realName = origName
        ,newName = nameNew
        ,AST.prologMode = plMode
        ,AST.bindType   = bType  }
      modify $ \s -> s 
        { localBindings = Map.insert origName uIdent $ localBindings s
        , visible       = Map.insert origName uIdent $ visible s
        , identDefinition = IntMap.insert
            (unNodeId nodeID) uIdent $ identDefinition s }


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

useIdent :: LIdent -> RM ()
useIdent lIdent = do
  vis <- lookupVisible lIdent
  case vis of
    Nothing -> throwError $ RenameError {
       renameErrorMsg = "Unbound Identifier :" ++ getOrigName lIdent
       ,renameErrorLoc = srcLoc lIdent }
    Just defIdent -> modify $ \s -> s
         { identUse =  IntMap.insert 
             (unNodeId $ nodeId lIdent) defIdent $ identUse s }
{-
rn just walks through the AST, without modifing it.
The actual renamings are stored in a sepearte AstAnnotation inside the RM-Monad
-}

nop :: RM ()
nop = return ()

rnModule :: ModuleFromParser -> RM ()
rnModule = rnDeclList . moduleDecls

rnExpList :: [LExp] -> RM ()
rnExpList = mapM_ rnExp

-- rename an expression
rnExp :: LExp -> RM ()
rnExp expression = case unLabel expression of
  Var ident -> useIdent ident
  IntExp _ -> nop
  SetExp a Nothing -> rnRange a
  SetExp a (Just comp) -> inCompGen comp (rnRange a)
  ListExp a Nothing -> rnRange a
  ListExp a (Just comp) -> inCompGen comp (rnRange a)
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
  NegExp a -> rnExp a
  Fun1 _ a -> rnExp a
  Fun2 _ a b -> rnExp a >> rnExp b
  DotTuple l -> rnExpList l
  Closure l -> rnExpList l
  ProcSharing al p1 p2 -> rnExp al >> rnExp p1 >> rnExp p2
  ProcAParallel a b c d -> rnExp a >> rnExp b >> rnExp c >> rnExp d
  ProcLinkParallel l e1 e2 -> rnLinkList l >> rnExp e1 >> rnExp e2
  ProcRenaming rlist gen proc -> case gen of
    Nothing -> mapM_ reRename rlist >> rnExp proc
    Just comp -> inCompGenL comp (mapM_ reRename rlist) >> rnExp proc
  ProcException p1 e p2 -> rnExp p1 >> rnExp e >> rnExp p2
  ProcRepSequence a p -> inCompGenL a (rnExp p)
  ProcRepInternalChoice a p -> inCompGenL a (rnExp p)
  ProcRepInterleave a p -> inCompGenL a (rnExp p)
  ProcRepExternalChoice  a p -> inCompGenL a (rnExp p)
  ProcRepAParallel comp a p -> inCompGenL comp (rnExp a >> rnExp p)
  ProcRepLinkParallel comp l p
    -> rnLinkList l >> inCompGenL comp (rnExp p)
  ProcRepSharing comp s p -> rnExp s >> inCompGenL comp (rnExp p)
  PrefixExp chan fields proc -> localScope $ do
    rnExp chan
    mapM_ rnCommField fields
    rnExp proc
{-
Catch these cases to make the match total.
These Constructors may only appear in later stages.
-}
  ExprWithFreeNames {} -> error "Rename.hs : no match for ExprWithFreeNames"
  LambdaI {} -> error "Rename.hs : no match for LambdaI"
  LetI {} -> error "Rename.hs : no match for LetI"
  PrefixI {} -> error "Rename.hs : no match for PrefixI"

rnRange :: LRange -> RM ()
rnRange r = case unLabel r of
  RangeEnum l -> rnExpList l
  RangeOpen a -> rnExp a
  RangeClosed a b -> rnExp a >> rnExp b

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
  ConstrPat {} -> error "Rename.hs : no match for ConstrPat" -- Where have they gone ?
  Selectors {} -> error "Rename.hs : no match for Selectors"
  Selector {} -> error "Rename.hs : no match for Selector"

rnCommField :: LCommField -> RM ()
rnCommField f = case unLabel f of
  InComm pat -> rnPattern pat
  InCommGuarded p g -> rnExp g >> rnPattern p
  OutComm e -> rnExp e

inCompGenL :: LCompGenList -> RM () -> RM ()
inCompGenL l r = inCompGen (unLabel l) r

inCompGen :: [LCompGen] -> RM () -> RM ()
inCompGen (h:t) ret = localScope $ do
  rnCompGen h
  inCompGen t ret
inCompGen [] ret = ret 

rnCompGen :: LCompGen -> RM ()
rnCompGen g = case unLabel g of
  Generator pat e -> rnExp e >> rnPattern pat
  Guard e -> rnExp e

reRename :: LRename -> RM ()
reRename (unLabel -> Rename e1 e2) = rnExp e1 >> rnExp e2

rnLinkList :: LLinkList -> RM ()
rnLinkList ll = case unLabel ll of
  LinkList l -> mapM_ rnLink l
  LinkListComprehension a b -> inCompGen a (mapM_ rnLink b)
  where
    rnLink (unLabel -> Link a b) = rnExp a >> rnExp b

-- rename a recursive binding group
rnDeclList :: [LDecl] -> RM ()
rnDeclList declList = do
  modify $ \s -> s {prologMode = PrologGround ,bindType   = LetBound}
  forM_ declList declLHS
  modify $ \s -> s {prologMode = PrologVariable ,bindType   = NotLetBound}
  forM_ declList declRHS

declLHS :: LDecl -> RM ()
declLHS d = case unLabel d of
  PatBind pat _ -> rnPattern pat
   --todo : proper type-checking/counting number of Funargs
  FunBind i _ -> bindNewUniqueIdent FunID i 
  Assert {} -> nop
  Transparent tl -> mapM_ (bindNewTopIdent TransparentID) tl
  SubType i clist -> do
    bindNewTopIdent DataTypeID i   -- fix this
    mapM_ rnSubtypeLHS clist
  DataType i clist -> do
    bindNewTopIdent DataTypeID i
    mapM_ rnConstructorLHS clist
  NameType i _ -> bindNewTopIdent NameTypeID i
  Channel chList _ -> mapM_ (bindNewTopIdent ChannelID) chList
  Print _ -> nop
  where
    rnConstructorLHS :: LConstructor -> RM ()
    rnConstructorLHS (unLabel -> Constructor c _)
      = bindNewTopIdent ConstrID c

    rnSubtypeLHS :: LConstructor -> RM ()
    rnSubtypeLHS (unLabel -> Constructor c _) = useIdent c


declRHS :: LDecl -> RM ()
declRHS d = case unLabel d of
  PatBind _ e -> rnExp e
  FunBind _ cases -> mapM_ rnFunCase cases
  Assert a -> case unLabel a of
      AssertBool e -> rnExp e
      AssertRefine _ p1 _ p2 -> rnExp p1 >> rnExp p2
      AssertTauPrio _ p1 _ p2 e -> rnExp p1 >> rnExp p2 >> rnExp e
      AssertModelCheck _ p _ _ -> rnExp p
  Transparent _ -> nop  
  SubType  _ clist -> forM_ clist rnConstructorRHS
  DataType _ clist -> forM_ clist rnConstructorRHS
  NameType _ td -> rnTypeDef td
  Channel _ Nothing -> nop
  Channel _ (Just td) -> rnTypeDef td
  Print e -> rnExp e
  where
    rnFunCase c = case c of  --todo:uses Labeled version
      FunCase pat e -> localScope (mapM_ rnPatList pat >> rnExp e)
      FunCaseI {} -> error "Rename.hs : no match for FunCaseI"
    rnConstructorRHS :: LConstructor -> RM ()
    rnConstructorRHS = rc . unLabel where
      rc (Constructor _ Nothing ) = nop
      rc (Constructor _ (Just t)) = rnTypeDef t

rnTypeDef :: LTypeDef -> RM ()
rnTypeDef t = case unLabel t of
  TypeTuple l -> rnExpList l
  TypeDot l -> rnExpList l

applyRenaming ::
     ModuleFromParser
  -> AstAnnotation UniqueIdent
  -> AstAnnotation UniqueIdent
  -> ModuleFromRenaming
applyRenaming ast defIdent usedIdent
  = castModule $ everywhere (mkT patchVarPat . mkT patchIdent) ast
  where
    patchIdent :: LIdent -> LIdent
    patchIdent l =
      let nodeID = unNodeId $ nodeId l in
      case (IntMap.lookup nodeID usedIdent, IntMap.lookup nodeID defIdent) of
        (Just use, _)  -> setNode l $ UIdent use
        (_, Just def)  -> setNode l $ UIdent def
        (Nothing, Nothing) -> error $
            "internal error: patchIdent nodeId not found:" ++ show nodeID
        (Just _ , Just _ ) -> error $
            "internal error: patchIdent nodeId is defining and using:" ++ show nodeID

    patchVarPat :: Pattern -> Pattern
    patchVarPat p@(VarPat x) = case idType $ unUIdent $ unLabel x of
        VarID -> p
        _ -> ConstrPat x
    patchVarPat x = x

-- | If a function is defined via pattern matching for serveral cases,
-- | the parser returns each case as an individual declaration.
-- | mergeFunBinds merges contiguous cases of the same function into one declaration.
mergeFunBinds :: ModuleFromParser -> ModuleFromParser
mergeFunBinds = everywhere (mkT patchModule . mkT patchLet)
  where
    patchModule :: ModuleFromParser -> ModuleFromParser
    patchModule m = m {moduleDecls = mergeDecls $ moduleDecls m}

    patchLet :: Exp -> Exp
    patchLet (Let decls expr) = Let (mergeDecls decls) expr
    patchLet x = x

    mergeDecls :: [LDecl] -> [LDecl]
    mergeDecls = map joinGroup . List.groupBy sameFunction

    sameFunction a b = case (unLabel a, unLabel b) of
       (FunBind n1 _, FunBind n2 _) -> unLabel n1 == unLabel n2
       _ -> False

    joinGroup :: [LDecl] -> LDecl
    joinGroup l@(firstCase : _) = case unLabel firstCase of
      FunBind fname _ -> setNode firstCase $ FunBind fname $ map getFunCase l
      _ -> firstCase
    joinGroup [] = error "unreachable : groupBy empty group ?"

    getFunCase :: LDecl -> FunCase
    getFunCase d = case unLabel d of
      FunBind _ [funCase] -> funCase
      FunBind _ _ -> error "mergeFunBinds: function already has several cases !"
      _ -> error "mergeFunBinds : internal error"