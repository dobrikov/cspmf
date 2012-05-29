-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.CompileAstToProlog
-- Copyright   :  (c) Fontaine, Dobrikov 2011
-- License     :  BSD3
-- 
-- Maintainer  :  fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Translation of an AST into Prolog terms, suitable for the ProB CSPM-Interpreter
-- 
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -Wall -Werror -fno-warn-warnings-deprecations #-}

module Language.CSPM.CompileAstToProlog
(
 cspToProlog
,mkSymbolTable
,mkSrcLoc
)
where

import Language.CSPM.Frontend (ModuleFromRenaming, frontendVersion)
import Language.CSPM.AST
import qualified Language.CSPM.SrcLoc as SrcLoc
import Language.Prolog.PrettyPrint.Direct

import Text.PrettyPrint
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.Version

-- | Translate a "LModule" into a "Doc" containing a number of Prolog facts.
-- The LModule must be a renamed,i.e. contain only unique "Ident"ifier.
cspToProlog ::
  ModuleFromRenaming -- ^ the renamed Module
  -> Doc  -- ^ prolog facts
cspToProlog ast = header $+$ core
  where
    core = mkModule ast
    header = vcat [
         text ":- dynamic channel/2, bindval/3, agent/3."
        ,text ":- dynamic agent_curry/3, symbol/4."
        ,text ":- dynamic dataTypeDef/2, subTypeDef/2, nameType/2."
        ,text ":- dynamic cspTransparent/1."
        ,text ":- dynamic cspPrint/1."
        ,text ":- dynamic pragma/1."
        ,text ":- dynamic comment/2."
        ,text ":- dynamic assertBool/1, assertRef/5, assertTauPrio/6."
        ,text ":- dynamic assertModelCheckExt/4, assertModelCheck/3."
        ]

plLocatedConstructs :: Set Const
plLocatedConstructs = Set.fromList 
  [F_Interleave , F_Interrupt, F_Timeout, F_CHAOS,
   F_ExtChoice, F_IntChoice, F_Sequential, F_Hiding
  ]

mkModule :: ModuleFromRenaming -> Doc
mkModule m
  = plPrg [
      singleClause $ clause $ nTerm "parserVersionNum"
        [pList $ map atom $ versionBranch $ frontendVersion]
     ,singleClause $ clause $ nTerm "parserVersionStr"
        [atom ("CSPM-Frontent-" ++ showVersion frontendVersion)]
     ,declGroup $ map clause $ declList $ moduleDecls m
     ,declGroup $ map mkPragma  $ modulePragmas m
     ,declGroup $ map mkComment $ moduleComments m
     ]

mkPragma :: String -> Clause
mkPragma s = clause $ nTerm "pragma" [aTerm s]

mkComment :: (Comment, SrcLoc.SrcLoc) -> Clause
mkComment (c, loc) = clause $ nTerm "comment" [com, mkSrcLoc loc]
  where
    com = case c of
      LineComment s ->  nTerm "lineComment" [aTerm s]
      BlockComment s -> nTerm "blockComment" [aTerm s]
      PragmaComment s -> nTerm "pragmaComment" [aTerm s]

te :: LExp -> Term
te expr = case unLabel expr of
  Var i -> let u = unUIdent $ unLabel i in
    case (prologMode u,idType u) of
      (PrologGround,VarID)   -> nTerm "val_of" [plNameTerm i, plLoc expr]
      _ -> plNameTerm i
  IntExp i -> nTerm "int" [atom i]
  SetExp r Nothing -> nTerm "setExp" [range r]
  SetExp r (Just comp) -> nTerm "setExp" [range r, comprehension comp]
  ListExp r Nothing -> nTerm "listExp" [range r]
  ListExp r (Just comp) -> nTerm "listExp" [range r, comprehension comp]
  ClosureComprehension (e,c) ->  nTerm "closureComp" [comprehension c ,eList e] 
  Let decl e -> nTerm "let" [pList $ declList decl, te e]
  Ifte cond t e -> nTerm "ifte" [te cond, te t, te e,condPos,thenPos,elsePos] where
    condPos = mkSrcLoc $ SrcLoc.srcLocFromTo (srcLoc expr) (srcLoc cond)
    thenPos = mkSrcLoc $ SrcLoc.srcLocBetween (srcLoc cond) (srcLoc t)
    elsePos = mkSrcLoc $ SrcLoc.srcLocBetween (srcLoc t) (srcLoc e)
  {- evil special case: ProB handles seq as builtin, but it is not -}
  CallFunction fkt args | (isSeq $ unLabel fkt) -> nTerm "builtin_call" [nTerm "seq" ( flatArgs args)]
    where
      isSeq (Var x) = (realName $ unUIdent $ unLabel x) == "seq"
      isSeq _ = False
  CallFunction fkt args -> case args of
     [l] -> nTerm "agent_call" [plLoc fkt, te fkt, eList l]
     (_:_:_) -> nTerm "agent_call_curry" [te fkt, pList $ map eList args ]
     [] -> error ("CallFunction without args" ++ show expr)
  CallBuiltIn builtIn args
    -> if ((unBuiltIn builtIn) `Set.member` plLocatedConstructs )
          then nTerm "builtin_call" [ nTerm (builtInToString builtIn) (plLoc expr : flatArgs args) ]
          else nTerm "builtin_call" [ nTerm (builtInToString builtIn) $ flatArgs args ]
  Lambda patl e -> nTerm "lambda" [pList $ map tp patl, te e]
  Stop  -> nTerm "stop" [plLoc expr]
  Skip  -> nTerm "skip" [plLoc expr]
  CTrue -> aTerm "true"
  CFalse -> aTerm "false"
  Events -> aTerm "Events"
  BoolSet -> aTerm "boolType"
  IntSet  -> aTerm "intType"
  TupleExp i -> nTerm "tupleExp" [eList i]
  Parens e -> term $ te e
  AndExp a b -> nTerm "bool_and" [te a, te b]
  OrExp a b -> nTerm "bool_or" [te a, te b]
  NotExp a -> nTerm "bool_not" [te a]
  NegExp a -> nTerm "negate" [te a]
  Fun1 op a -> nTerm (builtInToString op) [te a]
  Fun2 op a b -> if ((unBuiltIn op) `Set.member` plLocatedConstructs ) 
    then nTerm (builtInToString op) [te a, te b, nTerm "src_span_operator" [plLoc expr, plLoc op]]
    else nTerm (builtInToString op) [te a, te b]
  DotTuple a -> nTerm "dotTuple" [eList a]
  Closure l -> nTerm "closure" [ eList l]
  ProcSharing al p1 p2 -> nTerm "sharing" [te al, te p1, te p2,plLoc expr]
  ProcAParallel a1 a2 p1 p2
    -> nTerm "aParallel" [te a1, te p1, te a2, te p2, plLoc expr]
  ProcLinkParallel ll a b
    ->  nTerm "lParallel" [ linkList ll, te a, te b, plLoc ll ] 
  ProcRenaming ren Nothing p
    -> nTerm "procRenaming" [ renameList ren, te p, plLoc expr ]
  ProcRenaming ren (Just gen) p
    -> nTerm "procRenamingComp" [te p, comprehension $ unLabel gen, renameList ren]
  ProcException p1 e p2 -> nTerm "exception" [te p1, te e, te p2, plLoc expr]
  ProcRepSequence gen proc -> nTerm "repSequence" [comprehension $ unLabel gen, te proc, plLoc gen]
  ProcRepInternalChoice gen proc 
    -> nTerm "repInternalChoice" [comprehension $ unLabel gen, te proc, plLoc gen]
  ProcRepInterleave gen proc 
    -> nTerm "repInterleave" [comprehension $ unLabel gen, te proc, plLoc gen]
  ProcRepExternalChoice gen proc -> nTerm "repChoice" [comprehension $ unLabel gen, te proc, plLoc gen]
  ProcRepAParallel gen alph proc
    -> nTerm "procRepAPrallel" [comprehension $ unLabel gen, nTerm "pair" [te alph, te proc] ,plLoc gen]
  ProcRepLinkParallel gen links proc
    -> nTerm "procRepLinkPrallel" [linkList links, comprehension $ unLabel gen, te proc, plLoc gen]
  ProcRepSharing gen share proc
    -> nTerm "procRepSharing" [te share, comprehension $ unLabel gen, te proc, plLoc gen]
  PrefixExp ch fields proc -> nTerm "prefix" [plLoc ch, mkCommFields fields, te ch, te proc,prefixLoc ]
    where
      prefixLoc = mkSrcLoc $ SrcLoc.srcLocBetween
        (if null fields then srcLoc $ ch else srcLoc $ last fields)
        (srcLoc proc)
  PrefixI {} -> missingCase "PrefixI"
  ExprWithFreeNames {} -> missingCase "ExprWithFreeNames"
  LambdaI {} -> missingCase "LambdaI"
  LetI {} -> missingCase "LetI"
  where
    missingCase :: String -> Term
    missingCase s = error $ "missing case in te :" ++ s
    flatArgs :: [[LExp]] -> [Term]
    flatArgs l = concatMap (map te) l  

    comprehension :: [LCompGen] -> Term
    comprehension l = pList $ map (comp . unLabel ) l
      where
        comp (Guard e) = nTerm "comprehensionGuard" [te e]
        comp (Generator pat e) = nTerm "comprehensionGenerator" [tp pat, te e]

    linkList :: LLinkList -> Term
    linkList ll = case unLabel ll of
      LinkList l -> nTerm "linkList" [ pList $ map (mklink . unLabel) l ]
      LinkListComprehension gen l 
        -> nTerm "linkListComp" [ comprehension gen, pList $ map (mklink . unLabel) l ]
      where
        mklink (Link a b) = nTerm "link" [te a,te b]

    renameList :: [LRename] -> Term
    renameList l = pList $ map (mkRen . unLabel) l
      where
        mkRen (Rename a b) = nTerm "rename" [te a, te b]
    mkCommFields :: [LCommField] -> Term
    mkCommFields l = pList $ map (mkCF . unLabel) l
      where
        mkCF (InComm p) = nTerm "in" [tp p]
        mkCF (OutComm e) = nTerm "out" [te e]
        mkCF (InCommGuarded p e) = nTerm "inGuard" [tp p, te e]

    range :: LRange -> Term
    range r = case unLabel r of
      RangeOpen a -> nTerm "rangeOpen" [te a]
      RangeClosed a b -> nTerm "rangeClosed" [te a, te b]
      RangeEnum l -> nTerm "rangeEnum" [eList l]

eList :: [LExp] -> Term
eList l = pList h
  where h :: [Term]
        h = map te l

tp :: LPattern -> Term
tp pattern = case unLabel pattern of
  IntPat i -> nTerm "int" [atom i]
  TruePat -> aTerm "true"
  FalsePat -> aTerm "false"
  WildCard -> plWildCard
  VarPat i -> plNameTerm i
  ConstrPat i -> plNameTerm i
  Also l -> tpList "alsoPattern" l
  Append l ->  tpList "appendPattern" l
  DotPat l ->  tpList "dotpat" l
  SingleSetPat p -> tpList "singleSetPat" [p]
  EmptySetPat -> aTerm "emptySet"
  ListEnumPat l -> tpList "listPat" l
  TuplePat l -> tpList "tuplePat" l
  Selector {} -> error "missing case in tp : Selector"
  Selectors {} -> error "missing case in tp : Selectors"
  where
    tpList :: String -> [LPattern] -> Term
    tpList f l =  nTerm f [pList $ map tp l]

declList :: [LDecl] -> [Term]
declList l = concatMap td l

td :: LDecl -> [Term]
td decl = case unLabel decl of
  PatBind pat e -> [ nTerm "bindval" [tp pat, te e, plLoc decl]]
  FunBind fkt caseList -> map (mkFunBind fkt) caseList
  Assert e -> mkAssert e
  Transparent idList 
    -> [ nTerm "cspTransparent" [pList $ map plName idList] ]
  SubType i constrL  -> [ nTerm "subTypeDef"  [plNameTerm i, mkConstructorList constrL] ]
  DataType i constrL -> [ nTerm "dataTypeDef" [plNameTerm i, mkConstructorList constrL] ]
  NameType i t -> [ nTerm "nameType" [plNameTerm i, nTerm "type" [mkTypeDef t]] ]
  Channel ids tdef -> map (mkChannel tdef) ids
  Print e -> [ nTerm "cspPrint" [te e] ]
  where
    mkFunBind :: LIdent -> FunCase -> Term
    mkFunBind ident (FunCase pat e) = case pat of 
      [p] -> nTerm "agent" [
              nTerm (plName ident) $ map tp p
             ,te e
             ,plLoc e]
      l -> nTerm "agent_curry" [
              nTerm (plName ident) $ map (pList . map tp) l 
             ,te e
             ,plLoc e]
    mkFunBind _ (FunCaseI {}) = error "unexpected case in mkFunBind: FunCaseI"
    mkConstructorList :: [LConstructor] -> Term
    mkConstructorList l = pList $ map mkConstructor l
    mkConstructor :: LConstructor -> Term
    mkConstructor c = case unLabel c of
      Constructor i Nothing  -> nTerm "constructor" [plNameTerm i]
      Constructor i (Just t) -> nTerm "constructorC" [plNameTerm i, mkTypeDef t]

    mkTypeDef :: LTypeDef -> Term
    mkTypeDef t = case unLabel t of
      TypeTuple l -> nTerm "typeTuple" [eList l]
      TypeDot   l -> nTerm "dotTupleType" [eList l]
   
    mkChannel :: Maybe LTypeDef -> LIdent -> Term
    mkChannel Nothing  i = nTerm "channel" [ plNameTerm i, nTerm "type" [term $ atom "dotUnitType" ]]
    mkChannel (Just t) i = nTerm "channel" [ plNameTerm i, nTerm "type" [mkTypeDef t]]



    mkAssert :: LAssertDecl -> [Term]
    mkAssert ass = case unLabel ass of
      AssertBool e -> [ nTerm "assertBool" [te e] ]
      AssertRefine b p1 m p2
        -> [ nTerm "assertRef" [aTerm $ show b, te p1, termShow m, te p2, plLoc decl] ]
      AssertTauPrio b p1 m p2 e
        -> [ nTerm "assertTauPrio" [aTerm $ show b, te p1, termShow m, te p2, te e, plLoc decl] ]
      AssertModelCheck b p m (Just ext)
        -> [ nTerm "assertModelCheckExt" [aTerm $ show b, te p, termShow m, termShow ext] ]
      AssertModelCheck b p m Nothing
        -> [ nTerm "assertModelCheck" [aTerm $ show b, te p, termShow m ] ]
    termShow :: Show a => Labeled a -> Term
    termShow = aTerm . show . unLabel

plNameTerm :: LIdent -> Term
plNameTerm l
    = case (idType uIdent,prologMode uIdent) of
        (VarID,PrologVariable) -> plVar ("_" ++ uniquePlName uIdent)
        (VarID,PrologGround)   -> term $ atom $ uniquePlName uIdent
        _             -> term $ plName l
    where uIdent = unUIdent $ unLabel l

plName :: LIdent -> Atom
plName l
    = case idType uIdent of
         TransparentID -> atom $ realName uIdent
         VarID         -> error ("plName : " ++ show l)
         _             -> atom $ uniquePlName uIdent
    where uIdent = unUIdent $ unLabel l

uniquePlName :: UniqueIdent -> String
uniquePlName = newName


plLoc :: Labeled x -> Term
plLoc = mkSrcLoc . srcLoc

-- | Translate a source location to Prolog
mkSrcLoc :: SrcLoc.SrcLoc -> Term
mkSrcLoc loc =  case loc of
  SrcLoc.TokPos {} ->  nTerm "src_position" 
      [itt $ SrcLoc.getStartLine loc
      ,itt $ SrcLoc.getStartCol loc
      ,itt $ SrcLoc.getStartOffset loc
      ,itt $ SrcLoc.getTokenLen loc ]
  SrcLoc.TokSpan {} -> nTerm "src_span"
      [itt $ SrcLoc.getStartLine loc
      ,itt $ SrcLoc.getStartCol loc
      ,itt $ SrcLoc.getEndLine loc
      ,itt $ SrcLoc.getEndCol loc
      ,itt $ SrcLoc.getStartOffset loc
      ,itt $ SrcLoc.getTokenLen loc ]
  SrcLoc.FixedLoc {} -> nTerm "src_span"
      [itt $ SrcLoc.getStartLine loc
      ,itt $ SrcLoc.getStartCol loc
      ,itt $ SrcLoc.getEndLine loc
      ,itt $ SrcLoc.getEndCol loc
      ,itt $ SrcLoc.getStartOffset loc
      ,itt $ SrcLoc.getTokenLen loc ]
  _ -> term $ atom "no_loc_info_available"
  where 
    itt :: Int -> Term
    itt = term . iatom . fromIntegral
    iatom :: Integer -> Atom
    iatom = atom


-- | Translate a "AstAnnotation" with "UnqiueIdentifier" (i.e. a Symboltable)
-- into a "Doc" containing Prolog facts
mkSymbolTable :: AstAnnotation UniqueIdent -> Doc
mkSymbolTable ids 
  = plPrg [declGroup $ map mkSymbol $ IntMap.elems ids]
  where
  mkSymbol :: UniqueIdent -> Clause
  mkSymbol i = clause $ nTerm "symbol"
   [aTerm $ uniquePlName i
   ,aTerm $ realName i
   ,mkSrcLoc $ bindingLoc i
   ,aTerm $ pprintIDType i
   ]
  pprintIDType :: UniqueIdent -> String
  pprintIDType i = case idType i of
    ChannelID -> "Channel"
    NameTypeID -> "Nametype"
    FunID -> "Funktion or Process"
    ConstrID   -> "Constructor of Datatype"
    DataTypeID     -> "Datatype"
    TransparentID  -> "Transparent function"
    BuiltInID  -> "BuiltIn primitive"
    VarID -> case prologMode i of
      PrologGround -> "Ident (Groundrep.)"
      PrologVariable -> "Ident (Prolog Variable)"

-- | Map the abstract datatype LBuiltIn back to plain Strings for Prolog
builtInToString :: LBuiltIn -> String
builtInToString x = 
  let (BuiltIn bi) = unLabel x in
  case bi of
  F_STOP -> "STOP"     
  F_SKIP -> "SKIP"     
  F_true -> "true"     
  F_false -> "false"
  F_not -> "not"      
  F_and -> "and"      
  F_or -> "or"       
  F_Int -> "Int"      
  F_Bool -> "Bool"     
  F_Events -> "Events"   
  F_CHAOS -> "CHAOS"    
  F_union -> "union"    
  F_inter -> "inter"    
  F_diff -> "diff"     
  F_Union -> "Union"    
  F_Inter -> "Inter"    
  F_member -> "member"   
  F_card -> "card"     
  F_empty -> "empty"    
  F_set -> "set"      
  F_Set -> "Set"      
  F_Seq -> "Seq"      
  F_null -> "null"     
  F_head -> "head"     
  F_tail -> "tail"     
  F_concat -> "concat"   
  F_elem -> "elem"     
  F_length -> "length"   
  F_Concat -> "^"        
  F_Len2 -> "#"        
  F_Mult -> "*"        
  F_Div -> "/"        
  F_Mod -> "%"        
  F_Add -> "+"        
  F_Sub -> "-"      
  F_Eq -> "=="       
  F_NEq -> "!="       
  F_GE -> ">="       
  F_LE -> "<="       
  F_LT -> "<"        
  F_GT -> ">"        
  F_Guard -> "&"        
  F_Sequential -> ";"        
  F_Interrupt -> "/\\"      
  F_ExtChoice -> "[]"       
  F_Timeout -> "[>"       
  F_IntChoice -> "|~|"      
  F_Interleave -> "|||"      
  F_Hiding -> "\\"        

unBuiltIn :: LBuiltIn -> Const
unBuiltIn x =  let (BuiltIn fkt) = unLabel x in fkt
