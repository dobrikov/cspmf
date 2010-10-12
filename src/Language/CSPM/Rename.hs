{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.Rename
-- Copyright   :  (c) Fontaine 2008
-- License     :  BSD
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Compute the mapping between the using occurences and the defining occurences of all Identifier in a Module
-- Also decide whether to use ground or non-ground- representaions for the translation to Prolog.
{-
todo : check that we do not bind variables when we pattern match against
constructors : add a testcase for that
todo :: maybe use SYB for gathering the renaming
todo :: maybe also compute debruin-index/ freevariables
todo :: check idType in useIdent
fix topleveldecls to toplevel ? -> allready done by parser
-}
module Language.CSPM.Rename
  (
   getRenaming
  ,applyRenaming
  ,RenameError(..)
  )
where

import Language.CSPM.AST hiding (prologMode,bindType)
import qualified Language.CSPM.AST as AST
import qualified Language.CSPM.SrcLoc as SrcLoc

import Data.Generics.Schemes (everywhere)
import Data.Generics.Aliases (mkT)
import Data.Typeable (Typeable)
import Control.Exception (Exception)

import Control.Monad.Error
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap

-- | 'getRenaming' computes two 'AstAnnotation's.
-- The first one contains all the defining occurences of identifier
-- The second one contains all the using occurences of identitier.
-- 'getRename' returns an 'RenameError' if the 'Module' contains unbound
-- identifiers or illegal redefinitions.
getRenaming ::
     LModule 
  -> Either RenameError (Bindings,AstAnnotation UniqueIdent,AstAnnotation UniqueIdent)
getRenaming m
  = case execStateT (rnModule m) initialRState of
      Right state -> Right (visible state,identDefinition state, identUse state)
      Left e -> Left e

type RM x = StateT RState (Either RenameError) x

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
   ,bindType   :: BindType
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
   ,bindType        = NotLetBound
  }

data RenameError
  = RenameError {
   renameErrorMsg :: String
  ,renameErrorLoc :: SrcLoc.SrcLoc
  } deriving (Show,Typeable)

instance Exception RenameError


instance Error RenameError where
  noMsg = RenameError { renameErrorMsg = "no Messsage", renameErrorLoc = SrcLoc.NoLocation }
  strMsg m = RenameError { renameErrorMsg = m, renameErrorLoc = SrcLoc.NoLocation }

bindNewTopIdent :: IDType -> LIdent -> RM ()
bindNewTopIdent t i = do
  let (Ident origName) = unLabel i
  vis <- gets visible
  case Map.lookup origName vis of
    Nothing -> bindNewUniqueIdent t i
    Just _ -> throwError $ RenameError {
      renameErrorMsg = "Redefinition of toplevel name " ++ origName
     ,renameErrorLoc = srcLoc i }

bindNewUniqueIdent :: IDType -> LIdent -> RM ()
bindNewUniqueIdent iType lIdent = do
  let (Ident origName) = unLabel lIdent
  local <- gets localBindings
  {- check that we do not bind a variable twice i.e. in a pattern -}
  case Map.lookup origName local of
    Nothing -> return ()
    Just _ -> throwError $ RenameError {
       renameErrorMsg = "Redefinition of " ++ origName
       ,renameErrorLoc = srcLoc lIdent }
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
          renameErrorMsg = "Illigal reuse of Contructor " ++ origName
         ,renameErrorLoc = srcLoc lIdent }
     ChannelID -> throwError $ RenameError {
          renameErrorMsg = "Illigal reuse of Channel " ++ origName
         ,renameErrorLoc = srcLoc lIdent }
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

useIdent :: (Maybe IDType) -> LIdent -> RM ()
useIdent expectedType lIdent = do
  let (Ident origName) = unLabel lIdent
      nodeID = nodeId lIdent
  vis <- gets visible
  case Map.lookup origName vis of
    Nothing -> throwError $ RenameError {
       renameErrorMsg = "Unbound Identifier :" ++ origName
       ,renameErrorLoc = srcLoc lIdent }
    Just uniqueIdent -> do   -- todo check idType
       case expectedType of
         Nothing -> return ()
         Just t  -> when (t /= idType uniqueIdent) $ do
           throwError $ RenameError {
              renameErrorMsg = "Typeerror :" ++ origName
             ,renameErrorLoc = srcLoc lIdent }
       modify $ \s -> s
         { identUse =  IntMap.insert 
             (unNodeId nodeID) uniqueIdent $ identUse s }
       return ()

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
  Var ident -> useIdent Nothing ident
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
  ProcSet -> nop
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
  InCommGuarded p g -> rnPattern p >> rnExp g
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
rnDeclList declList = do
  modify $ \s -> s {prologMode = PrologGround
                   ,bindType   = LetBound}
  forM_ declList declLHS
  modify $ \s -> s {prologMode = PrologVariable
                   ,bindType   = NotLetBound}
  forM_ declList declRHS

declLHS :: LDecl -> RM ()
declLHS d = case unLabel d of
  PatBind pat _ -> rnPattern pat
   --todo : proper type-checking/counting number of Funargs
  FunBind i _ -> bindNewUniqueIdent (FunID (-1)) i
  AssertRef {} -> nop
  AssertBool {} -> nop
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
    rnConstructorLHS a = do
      let (Constructor c _ ) = unLabel a
      bindNewTopIdent (ConstrID "someConstructor") c --Todo -- fix

    rnSubtypeLHS :: LConstructor -> RM ()
    rnSubtypeLHS a = do
      let (Constructor c _ ) = unLabel a
      useIdent Nothing c -- <- fix this Nothing <-> dont check

declRHS :: LDecl -> RM ()
declRHS d = case unLabel d of
  PatBind _ e -> rnExp e
  FunBind _ cases -> mapM_ rnFunCase cases
  AssertRef a _ b -> rnExp a >> rnExp b
  AssertBool e -> rnExp e
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

-- | 'applyRenaming' uses SYB to replace turn every 'Ident' in the 'Module' into to the
-- 'UIdent' version, i.e. set the 'UniqueIdent'.
-- At the same time, we also replace VarPat x with ConstrPat x if x an toplevel constant
-- It is an error if the 'Module' contains occurences of 'Ident' that are not covered by
-- the 'AstAnnotation's.
applyRenaming ::
     (Bindings,AstAnnotation UniqueIdent,AstAnnotation UniqueIdent)
  -> LModule 
  -> LModule
applyRenaming (_,defIdent,usedIdent) ast
  = everywhere (mkT patchVarPat . mkT patchIdent) ast
  where
    patchIdent :: LIdent -> LIdent
    patchIdent l =
      let nodeID = unNodeId $ nodeId l in
      case IntMap.lookup nodeID usedIdent of
        Just u -> l { unLabel = UIdent u}
        Nothing -> case IntMap.lookup nodeID defIdent of
          Just d -> l { unLabel = UIdent d}
          Nothing -> error $ "internal error: patchIdent nodeId :" ++ show nodeID

    patchVarPat :: Pattern -> Pattern
    patchVarPat p@(VarPat x) = case idType $ unUIdent $ unLabel x of
        VarID -> p
        _ -> ConstrPat x
    patchVarPat x = x