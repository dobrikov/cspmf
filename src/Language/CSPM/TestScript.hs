{-# LANGUAGE TypeSynonymInstances, FlexibleInstances#-}

module Language.CSPM.TestScript
where

import Language.CSPM.AST
import Language.CSPM.Utils(parseFile)
import Language.CSPM.PrettyPrinter(dropCsp, toPrettyString)
import Language.CSPM.AstUtils(removeParens, removeModuleTokens, removeSourceLocations, unUniqueIdent)
import Language.CSPM.SrcLoc

testPrettyParser :: FilePath -> IO Bool
testPrettyParser f = 
  do 
   str1 <- parseFile f
   let fileName = dropCsp f
   writeFile (fileName ++ "Pretty.csp") (toPrettyString str1)
   str2 <- parseFile (fileName ++ "Pretty.csp") 
   return $ compareAST (simplifyAst str1) (simplifyAst str2)

simplifyAst :: LModule -> LModule
simplifyAst ast = removeParens $ removeSourceLocations $ removeModuleTokens ast

class EqAst a where
   compareAST :: a -> a -> Bool

instance (EqAst t) => EqAst (Labeled t) where
   compareAST a b = compareAST (unLabel a) (unLabel b)

instance EqAst Module where
   compareAST m1 m2 = and $ zipWith compareAST (moduleDecls m1) (moduleDecls m2)

instance EqAst Ident where
   compareAST a b = case (a,b) of
     (Ident  _, Ident _)  -> (unIdent a) == (unIdent b)
     (UIdent _, UIdent _) -> (realName $ unUIdent a) == (realName $ unUIdent b)
     (UIdent _, Ident _)  -> (realName $ unUIdent a) == (unIdent b)
     (Ident  _, UIdent _) -> (unIdent a) == (realName $ unUIdent b)

instance EqAst Pattern where
   compareAST (IntPat a) (IntPat b) = a == b
   compareAST TruePat TruePat       = True
   compareAST FalsePat FalsePat     = True
   compareAST WildCard WildCard     = True
   compareAST EmptySetPat EmptySetPat     = True
   compareAST (ConstrPat a) (ConstrPat b) = compareAST a b
   compareAST (Also lista) (Also listb) = and $ zipWith compareAST lista listb
   compareAST (Append lista) (Append listb) = and $ zipWith compareAST lista listb
   compareAST (DotPat lista) (DotPat listb) = and $ zipWith compareAST lista listb
   compareAST (ListEnumPat lista) (ListEnumPat listb) = and $ zipWith compareAST lista listb
   compareAST (TuplePat lista) (TuplePat listb) = and $ zipWith compareAST lista listb
   compareAST (VarPat a) (VarPat b) = compareAST a b
   compareAST (SingleSetPat a) (SingleSetPat b) = compareAST a b    
   compareAST _  _                  = False

instance EqAst Exp where
   compareAST (Var a) (Var b) = compareAST a b
   compareAST (IntExp a) (IntExp b)       = a == b
   compareAST Stop Stop     = True
   compareAST Skip Skip     = True
   compareAST CTrue CTrue   = True
   compareAST CFalse CFalse = True
   compareAST Events Events   = True
   compareAST BoolSet BoolSet = True
   compareAST IntSet IntSet   = True
   compareAST ProcSet ProcSet = True
   compareAST (Parens a) (Parens b) = compareAST a b
   compareAST (AndExp a1 a2) (AndExp b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST (OrExp a1 a2) (OrExp b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST (NotExp a) (NotExp b) = compareAST a b
   compareAST (NegExp a) (NegExp b) = compareAST a b
   compareAST (DotTuple lista) (DotTuple listb) = and $ zipWith compareAST lista listb
   compareAST (TupleExp lista) (TupleExp listb) = and $ zipWith compareAST lista listb
   compareAST (Closure lista) (Closure listb) = and $ zipWith compareAST lista listb
   compareAST (Let lista a) (Let listb b) = and [(and $ zipWith compareAST lista listb), compareAST a b]
   compareAST (Ifte a1 a2 a3) (Ifte b1 b2 b3) = and [compareAST a1 b1, compareAST a2 b2, compareAST a3 b3]
   compareAST (Lambda lista a) (Lambda listb b) = and [(and $ zipWith compareAST lista listb), compareAST a b]
   compareAST (Fun1 a1 a2) (Fun1 b1 b2) = and [compareAST a1 b1, compareAST a2 b2] 
   compareAST (Fun2 a1 a2 a3) (Fun2 b1 b2 b3) = and [compareAST a1 b1, compareAST a2 b2, compareAST a3 b3] 
   compareAST (SetExp rangea a) (SetExp rangeb b) = case (a, b) of
      (Just t1, Just t2) -> and [(and $ zipWith compareAST t1 t2), compareAST (unLabel rangea) (unLabel rangeb)]
      (Nothing, Nothing) -> compareAST rangea rangeb
      otherwise -> False
   compareAST (ListExp rangea a) (ListExp rangeb b) = case (a, b) of
      (Just t1, Just t2) -> and [(and $ zipWith compareAST t1 t2), compareAST (unLabel rangea) (unLabel rangeb)]
      (Nothing, Nothing) -> compareAST rangea rangeb
      otherwise -> False
   compareAST (ClosureComprehension (l1, l2)) (ClosureComprehension (ll1, ll2)) =  and [and $ zipWith compareAST l1 ll1, and $ zipWith compareAST l2 ll2]
   compareAST (CallFunction a [lista]) (CallFunction b [listb]) = and [(and $ zipWith compareAST lista listb), compareAST a b]
   compareAST (CallBuiltIn a [lista]) (CallBuiltIn b [listb]) = and [and $ zipWith compareAST lista listb, compareAST a b]
   compareAST (ProcSharing a1 a2 a3) (ProcSharing b1 b2 b3) = and [compareAST a1 b1, compareAST a2 b2, compareAST a3 b3] 
   compareAST (ProcAParallel a1 a2 a3 a4) (ProcAParallel b1 b2 b3 b4) = and [compareAST a1 b1, compareAST a2 b2, compareAST a3 b3, compareAST a4 b4] 
   compareAST (ProcLinkParallel a1 a2 a3) (ProcLinkParallel b1 b2 b3) = and [compareAST a1 b1, compareAST a2 b2, compareAST a3 b3] 
   compareAST (ProcRepLinkParallel a1 a2 a3) (ProcRepLinkParallel b1 b2 b3) = and [compareAST a1 b1, compareAST a2 b2, compareAST a3 b3] 
   compareAST (ProcRepSharing a1 a2 a3) (ProcRepSharing b1 b2 b3) = and [compareAST a1 b1, compareAST a2 b2, compareAST a3 b3] 
   compareAST (ProcRepAParallel a1 a2 a3) (ProcRepAParallel b1 b2 b3) = and [compareAST a1 b1, compareAST a2 b2, compareAST a3 b3] 
   compareAST (ProcException a1 a2 a3) (ProcException b1 b2 b3) = and [compareAST a1 b1, compareAST a2 b2, compareAST a3 b3] 
   compareAST (ProcRepInterleave a1 a2) (ProcRepInterleave b1 b2) = and [compareAST a1 b1, compareAST a2 b2] 
   compareAST (ProcRepExternalChoice a1 a2) (ProcRepExternalChoice b1 b2) = and [compareAST a1 b1, compareAST a2 b2] 
   compareAST (ProcRepInternalChoice a1 a2) (ProcRepInternalChoice b1 b2) = and [compareAST a1 b1, compareAST a2 b2] 
   compareAST (ProcRepSequence a1 a2) (ProcRepSequence b1 b2) = and [compareAST a1 b1, compareAST a2 b2] 
   compareAST (PrefixExp a1 lista a2) (PrefixExp b1 listb b2) = and [compareAST a1 b1, compareAST a2 b2, and $ zipWith compareAST lista listb] 
   compareAST (ProcRenaming lista ma a) (ProcRenaming listb mb b) = case (ma, mb) of
      (Just t1, Just t2) -> and [compareAST t1 t2, and $ zipWith compareAST lista listb, compareAST a b]
      (Nothing, Nothing) -> and [and $ zipWith compareAST lista listb, compareAST a b]
      otherwise -> False 
   compareAST _ _ = False

instance EqAst Range where
   compareAST (RangeEnum lista) (RangeEnum listb) = and $ zipWith compareAST lista listb
   compareAST (RangeClosed a1 a2) (RangeClosed b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST (RangeOpen a) (RangeOpen b) = compareAST a b
   compareAST _ _ = False

instance EqAst CommField where
   compareAST (InComm a) (InComm b) = compareAST a b
   compareAST (InCommGuarded a1 a2) (InCommGuarded b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST (OutComm a) (OutComm b) = compareAST a b
   compareAST _ _ = False
   
instance EqAst LinkList where
   compareAST (LinkList lista) (LinkList listb) = and $ zipWith compareAST lista listb
   compareAST (LinkListComprehension lista1 lista2) (LinkListComprehension listb1 listb2) = and [and $ zipWith compareAST lista1 listb1, and $ zipWith compareAST lista2 listb2]
   compareAST _ _ = False

instance EqAst Link where
   compareAST (Link a1 a2) (Link b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST _ _ = False

instance EqAst Rename where
   compareAST (Rename a1 a2) (Rename b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST _ _ = False

instance EqAst BuiltIn where
   compareAST (BuiltIn a) (BuiltIn b) = compareAST a b
   compareAST _ _ = False

instance EqAst CompGen where
   compareAST (Guard a) (Guard b) = compareAST a b
   compareAST (Generator a1 a2) (Generator b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST _ _ = False

instance EqAst [LCompGen] where
   compareAST a b = and $ zipWith compareAST a b

instance EqAst Decl where
   compareAST (PatBind a1 a2) (PatBind b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST (FunBind a1 lista) (FunBind b1 listb) = and [compareAST a1 b1, and $ zipWith compareAST lista listb]
   compareAST (AssertRef a1 a2 a3 _) (AssertRef b1 b2 b3 _) = and [compareAST a1 b1, a2 == b2, compareAST a3 b3]
   compareAST (AssertBool a) (AssertBool b) = compareAST a b
   compareAST (Transparent lista) (Transparent listb) = and $ zipWith compareAST lista listb
   compareAST (SubType a lista) (SubType b listb) = and [compareAST a b, and $ zipWith compareAST lista listb]
   compareAST (DataType a lista) (DataType b listb) = and [compareAST a b, and $ zipWith compareAST lista listb]
   compareAST (NameType a1 a2) (NameType b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST (Channel lista a) (Channel listb b) = case (a,b) of
      (Just t1, Just t2) -> and [compareAST t1 t2, and $ zipWith compareAST lista listb]
      (Nothing, Nothing) -> and $ zipWith compareAST lista listb
      otherwise          -> False
   compareAST (Print a) (Print b) = compareAST a b
   compareAST _ _ = False

instance EqAst FunArgs where
   compareAST [a] [b] = and $ zipWith compareAST a b
   compareAST _ _ = False

instance EqAst FunCase where
   compareAST (FunCase a1 a2) (FunCase b1 b2) = and [compareAST a1 b1, compareAST a2 b2]
   compareAST (FunCaseI lista a) (FunCaseI listb b) = and [and $ zipWith compareAST lista listb, compareAST a b]
   compareAST _ _ = False
instance EqAst TypeDef where
   compareAST (TypeTuple a) (TypeTuple b) = and $ zipWith compareAST a b
   compareAST (TypeDot a) (TypeDot b) = and $ zipWith compareAST a b   
   compareAST _ _ = False

instance EqAst Constructor where
   compareAST (Constructor a ma) (Constructor b mb) = case (ma, mb) of
     (Just t1, Just t2) -> and [compareAST a b, compareAST t1 t2]
     (Nothing, Nothing) -> compareAST a b
     otherwise -> False
   compareAST _ _ = False

instance EqAst Const where
    compareAST F_true F_true = True
    compareAST F_false F_false = True
    compareAST F_not F_not = True
    compareAST F_and F_and = True
    compareAST F_or F_or = True
    compareAST F_union F_union = True
    compareAST F_inter F_inter = True
    compareAST F_diff F_diff = True
    compareAST F_Union F_Union = True
    compareAST F_Inter F_Inter = True
    compareAST F_member F_member = True
    compareAST F_card F_card = True
    compareAST F_empty F_empty = True
    compareAST F_set F_set = True
    compareAST F_Set F_Set = True
    compareAST F_Seq F_Seq = True
    compareAST F_null F_null = True
    compareAST F_head F_head = True
    compareAST F_tail F_tail = True
    compareAST F_concat F_concat = True
    compareAST F_elem F_elem = True
    compareAST F_length F_length = True
    compareAST F_STOP F_STOP = True
    compareAST F_SKIP F_SKIP = True
    compareAST F_Events F_Events = True
    compareAST F_Int F_Int = True
    compareAST F_Bool F_Bool = True
    compareAST F_CHAOS F_CHAOS = True
    compareAST F_Concat F_Concat = True
    compareAST F_Len2 F_Len2 = True
    compareAST F_Mult F_Mult = True
    compareAST F_Div F_Div = True
    compareAST F_Mod F_Mod = True
    compareAST F_Add F_Add = True
    compareAST F_Sub F_Sub = True
    compareAST F_Eq F_Eq = True
    compareAST F_NEq F_NEq = True
    compareAST F_GE F_GE = True
    compareAST F_LE F_LE = True
    compareAST F_LT F_LT = True
    compareAST F_GT F_GT = True
    compareAST F_Guard F_Guard = True
    compareAST F_Sequential F_Sequential = True
    compareAST F_Interrupt F_Interrupt = True
    compareAST F_ExtChoice F_ExtChoice = True
    compareAST F_IntChoice F_IntChoice = True
    compareAST F_Hiding F_Hiding = True
    compareAST F_Timeout F_Timeout = True
    compareAST F_Interleave F_Interleave = True

