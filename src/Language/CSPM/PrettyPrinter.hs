----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.PrettyPrinter
-- Copyright   :  (c) Ivaylo Dobrikov 2010
-- License     :  BSD
-- 
-- Maintainer  :  Ivaylo Dobrikov (me@dobrikov.biz)
-- Stability   :  experimental
-- Portability :  GHC-only
-- 
-- This module defines functions for pretty-printing the 
-- Abstract Syntax Tree to CSPM syntax.
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.CSPM.PrettyPrinter
(
  pPrint
)
where

import Text.PrettyPrint.HughesPJClass

import Language.CSPM.AST

instance (Pretty x) => Pretty (Labeled x) where
  pPrint = pPrint . unLabel

instance Pretty (Module a) where
  pPrint m = vcat $ map pPrint (moduleDecls m)


-- help functions for the Instances of the Type-class Pretty
dot :: Doc
dot = text "."

pPrintListSet :: (Pretty r) => String -> String -> r -> Maybe [LCompGen] -> Doc
pPrintListSet str1 str2 range mgen  =
     case mgen of
       Nothing  -> text str1 <+>  pPrint range <+>  text str2
       Just gen -> text str1 <+> pPrint range <+> text "|"
                    <+> (hsep $ punctuate comma (map (pPrintCompGen False) gen)) <+> text str2

hsepPrintunctuate :: (Pretty t) => Doc -> [t] -> Doc
hsepPrintunctuate s l = hsep $ punctuate s $ map pPrint l

hcatPunctuate :: (Pretty t) => Doc -> [t] -> Doc
hcatPunctuate s l = hcat $ punctuate s $ map pPrint l

printFunBind :: LIdent -> [FunCase] -> Doc
printFunBind ident lcase = vcat $ map (printIdent (unLabel ident) <>) (map printCase lcase) 

printCase :: FunCase -> Doc
printCase c = case c of
  FunCaseI pat  expr -> (parens $ hcatPunctuate comma pat) <+> equals <+> pPrint expr
  FunCase  list expr -> (hcat $ map mkPat list) <+> equals <+> pPrint expr
  where
    mkPat l = parens $ hcatPunctuate comma l

instance Pretty Decl where
  pPrint d = case d of
    PatBind pat expr -> pPrint pat  <+> equals <+> (pPrint expr)
    FunBind ident lcase -> printFunBind ident lcase
    Assert a -> text "assert" <+> pPrint a
    Transparent ids
      -> text "transparent" <+> (hsep $ punctuate comma (map (printIdent . unLabel) ids))
    SubType ident constrs
      ->     text "subtype" <+> printIdent (unLabel ident) <+> equals
         <+> (vcat $ punctuate (text "|") (map printConstr (map unLabel constrs)))
    DataType ident constrs
      ->     text "datatype" <+> printIdent (unLabel ident) <+> equals 
         <+> (hsep $ punctuate (text "|") (map printConstr (map unLabel constrs)))
    NameType ident typ
      -> text "nametype" <+> printIdent (unLabel ident) <+> equals <+> typeDef typ
    Channel ids t
      -> text "channel" <+> (hsep $ punctuate comma $ map (printIdent . unLabel) ids) <+> typ
       where
         typ = case t of
           Nothing -> empty
           Just x -> text ":" <+> typeDef x
    Print expr -> text "print"  <+> pPrint expr

printFunArgs :: [[LExp]] -> Doc
printFunArgs = hcat . map (parens . hsepPrintunctuate comma)

-- Contructors
printConstr :: Constructor -> Doc
printConstr (Constructor ident typ) = printIdent (unLabel ident) <>
  case typ of 
   Nothing -> empty
   Just t  -> dot <> typeDef  t

-- Type Definitions
typeDef :: LTypeDef -> Doc
typeDef typ = case unLabel typ of
  TypeTuple e -> parens $ hcatPunctuate comma e
  TypeDot e -> hcatPunctuate dot e

instance Pretty Exp where
  pPrint expression = case expression of
    Var ident -> printIdent $ unLabel ident
    IntExp i -> integer i
    SetExp range mgen -> pPrintListSet "{"  "}" range mgen
    ListExp range mgen -> pPrintListSet "<"  ">" range mgen
    ClosureComprehension (lexp,lcomp)
      -> pPrintListSet "{|" "|}" (labeled $ RangeEnum lexp) (Just lcomp)
    Let ldecl expr -> vcat
      [
       nest 2 (text "let")
      ,vcat $ punctuate (text "" $$ nest 4 (text "")) (map pPrint ldecl)
      ,nest 2 (text "within" <+> pPrint expr)
      ]
    Ifte expr1 expr2 expr3 -> vcat
      [
       nest 2 $ text "if" <+> pPrint expr1
      ,nest 4 $ text "then" <+> pPrint expr2
      ,nest 4 $ text "else" <+> pPrint expr3
      ]
    CallFunction expr list -> pPrint expr  <> printFunArgs list
    CallBuiltIn  builtin [expr] -> pPrint builtin <> (parens $ hsepPrintunctuate comma expr)
    CallBuiltIn  _ _ -> error "pPrint Exp: builtin must have exactly one argument"
    Lambda pat expr -> text "\\" <+> hsepPrintunctuate comma pat <+> text "@" <+> pPrint expr
    Stop    -> text "STOP"
    Skip    -> text "SKIP"
    CTrue   -> text "true"
    CFalse  -> text "false"
    Events  -> text "Events"
    BoolSet -> text "Bool"
    IntSet  -> text "Int"
    TupleExp e      -> parens $ hsepPrintunctuate comma e
    Parens e        -> parens $ pPrint e
    AndExp a b      -> pPrint a <+> text "and" <+> pPrint b
    OrExp a b       -> pPrint a<+> text "or" <+> pPrint b
    NotExp e        -> text "not" <+> pPrint e
    NegExp e        -> text " " <> text "-" <>  pPrint e
    Fun1 builtin e  -> pPrint builtin <> (parens $ pPrint e)
    Fun2 builtin a b -> pPrint a <+> pPrint builtin <+> pPrint b
    DotTuple l      -> hcatPunctuate dot l
    Closure e       -> text "{|" <+> hsepPrintunctuate comma e <+> text "|}"

-- process expressions
    ProcSharing e p1 p2 -> pPrint p1 <> text "[|" <+> pPrint e <+> text "|]" <> pPrint p2
    ProcAParallel expr1 expr2  p1 p2
      -> pPrint p1 <> (brackets $ pPrint expr1 <+> text "||" <+> pPrint expr2) <> pPrint p2
    ProcLinkParallel llist p1 p2 -> pPrint p1 <> pPrint llist <> pPrint p2
    ProcRenaming renames mgen proc
      -> pPrint proc  <> text "[[" <+> hsepPrintunctuate comma renames <+> gens <+> text "]]"
         where
           gens = case mgen of
                    Nothing   -> empty
                    Just lgen -> text "|" <+> (separateGen False (unLabel lgen))
                                                                          
    ProcException e p1 p2 -> pPrint p1 <+> text "[|" <+> pPrint e <+> text "|>" <+> pPrint p2
    ProcRepSequence lgen proc -> replicatedProc (text ";")   (unLabel lgen) proc
    ProcRepInternalChoice lgen proc -> replicatedProc (text "|~|") (unLabel lgen) proc
    ProcRepExternalChoice lgen proc -> replicatedProc (text "[]")  (unLabel lgen) proc
    ProcRepInterleave lgen proc -> replicatedProc (text "|||") (unLabel lgen) proc
    PrefixExp expr fields proc
        -> pPrint expr <> (hcat $ map pPrint fields) <+> text "->" <+> pPrint proc
    ProcRepSharing lgen expr proc
        ->     text "[|" <+> pPrint expr <+> text "|]"
           <+> (separateGen True (unLabel lgen)) <+> text "@" <+> pPrint proc
    ProcRepAParallel lgen expr proc
        -> text "||" <+> (separateGen True (unLabel lgen)) <+> text "@" 
                                                  <+> (brackets $ pPrint expr) <+> pPrint proc
    ProcRepLinkParallel lgen llist proc
        -> pPrint llist  <+> (separateGen True (unLabel lgen)) <+> text "@" <+> pPrint proc

-- only used in later stages
-- this do not affect the CSPM notation: same outputs as above
    PrefixI _ expr fields proc -> pPrint expr <> (hcat $ map pPrint fields) <+> text "->" <+> pPrint proc
    LetI decls _ expr -> hcat
       [
        nest 2 $ text "let"
       ,nest 4 $ hcat $ map pPrint decls
       ,nest 2 $ text "within" <+> pPrint expr
       ]
    LambdaI _ pat expr
        -> text "\\" <+> hsepPrintunctuate comma pat <+> text "@" <+> pPrint expr
    ExprWithFreeNames _ expr -> pPrint expr

replicatedProc :: Doc -> [LCompGen] -> LProc -> Doc
replicatedProc op lgen proc = op <+> (separateGen True lgen) <+> text "@" <+> pPrint proc

instance Pretty LinkList where
  pPrint (LinkList list)                   = brackets $ hsepPrintunctuate comma list
  pPrint (LinkListComprehension lgen list)
    = brackets (hsepPrintunctuate comma list <+> text "|" <+> separateGen False lgen)

instance Pretty Link where
  pPrint (Link expr1 expr2) = pPrint expr1 <+> text "<->" <+> pPrint expr2

instance Pretty Rename where
  pPrint (Rename expr1 expr2) = pPrint expr1 <+> text "<-" <+> pPrint expr2

separateGen :: Bool -> [LCompGen] -> Doc
separateGen b lgen = hsep $ punctuate comma $ map (pPrintCompGen b) lgen

-- the generators of the comprehension sets, lists (all after the |) and 
-- inside replicated processes (like "x: {1..10}", in this case the bool variable must be true,
-- otherwise false)
pPrintCompGen :: Bool -> LCompGen -> Doc
pPrintCompGen b gen = case unLabel gen of
  (Generator pat expr) -> (pPrint pat) <+> case b of
           False -> text "<-" <+> (pPrint expr)
           True  -> text ":"  <+> (pPrint expr)
  (Guard expr)         -> pPrint expr

-- the range of sets and lists
instance Pretty Range where
  pPrint r = case r of
    RangeEnum expr -> hsepPrintunctuate comma expr
    RangeClosed a b -> pPrint a <> text ".." <> pPrint b
    RangeOpen expr -> pPrint expr <> text ".."

-- unwrapPrint the BuiltIn-oparator
instance Pretty BuiltIn where
  pPrint (BuiltIn c) = pPrint c

-- the communication fields
instance Pretty CommField where
  pPrint (InComm pat)            = text "?" <> pPrint pat
  pPrint (InCommGuarded pat expr) = text "?" <> pPrint pat <> text ":" <> pPrint expr
  pPrint (OutComm expr)           = text "!" <> pPrint expr

-- pretty-printing for CSPM-Patterns
instance Pretty Pattern where
  pPrint pattern = case pattern of
    IntPat n         -> integer n
    TruePat          -> text "true"
    FalsePat         -> text "false"
    WildCard         -> text "_"
    ConstrPat ident  -> printIdent $ unLabel ident
    Also pat         -> pPrintAlso (Also pat)
    Append pat       -> hcatPunctuate (text "^") pat
    DotPat []        -> error "pPrint Pattern: empty dot pattern"
    DotPat [pat]     -> pPrint pat
    DotPat l         -> hcat $ punctuate dot $ map nestedDotPat l
    SingleSetPat pat -> text "{" <+> (pPrint pat) <+> text "}"
    EmptySetPat      -> text "{ }"
    ListEnumPat pat  -> text "<" <+> hsepPrintunctuate comma pat <+> text ">"
    TuplePat pat     -> text "(" <> hsepPrintunctuate comma pat <>  text ")"
    VarPat ident     -> printIdent $ unLabel ident
    Selectors _ _    -> error "pPrint Pattern Seclectors"
    Selector _ _     -> error "pPrint Pattern Seclector"
    where
      nestedDotPat p = case unLabel p of
        DotPat {} -> parens $ pPrint p
        x -> pPrint x

-- external function for Also-Patterns for a better look
pPrintAlso :: Pattern -> Doc
pPrintAlso (Also [])    = text ""
pPrintAlso (Also (h:t)) =
   case unLabel h of
     DotPat _ -> if length t > 0 then (pPrint h) <> text "@@" <> pPrintAlso (Also t)
                                 else pPrint h
     Append _ -> if length t > 0 then (pPrint h) <> text "@@" <> pPrintAlso (Also t)
                                 else pPrint h
     _        -> if length t > 0 then pPrint h <> text "@@" <> pPrintAlso (Also t)
                                 else pPrint h
pPrintAlso _ = text ""

-- disticts the cases for different syntax-records for the Ident datatype
printIdent :: Ident -> Doc
printIdent ident = 
  case ident of 
   Ident _  -> text $ unIdent ident
   UIdent _ -> text $ newName $ unUIdent ident

instance Pretty AssertDecl where
  pPrint a = case a of
     AssertBool expr -> pPrint expr
     AssertRefine n expr1 op expr2
       -> negated n $ pPrint expr1 <+> pPrint op <+> pPrint expr2
     AssertTauPrio n expr1 op expr2 expr3
       -> negated n $ pPrint expr1 <+> pPrint op <+> pPrint expr2 <+> text ":[tau priority over]:" <+> pPrint expr3
     AssertModelCheck n expr m mb
       -> negated n $ pPrint expr <+> text ":[" <+> pPrint m <+> maybe empty pPrint mb <+> text "]"
    where
      negated ar doc = if ar then text "not" <+> doc else doc

instance Pretty FdrExt where
  pPrint i = case i of
    F  -> text "[F]"
    FD -> text "[FD]"
    T  -> text "[T]"

instance Pretty FDRModels where
  pPrint m = case m of
    DeadlockFree  -> text "deadlock free"
    Deterministic -> text "deterministic"
    LivelockFree  -> text "livelock free"

instance Pretty RefineOp where
  pPrint x = case x of
    Trace -> text "[T="
    Failure -> text "[F="
    FailureDivergence -> text "[FD="
    RefusalTesting -> text "[R="
    RefusalTestingDiv -> text "[RD="
    RevivalTesting -> text "[V="
    RevivalTestingDiv -> text "[VD="
    TauPriorityOp -> text "[TP="

instance Pretty TauRefineOp where
  pPrint TauTrace = text "[T="
  pPrint TauRefine = text "[="

instance Pretty Const where
  pPrint x = case x of
-- Booleans
    F_true   -> text "true"
    F_false  -> text "false"
    F_not    -> text "not"
    F_and    -> text "and"
    F_or     -> text "or"
-- Numbers
    F_Mult   -> text "*"
    F_Div    -> text "/"
    F_Mod    -> text "%"
    F_Add    -> text "+"
    F_Sub    -> text "" <+> text "-"
-- Equality
    F_GE     -> text ">="
    F_LE     -> text "<="
    F_LT     -> text "<"
    F_GT     -> text ">"
    F_Eq     -> text "=="
    F_NEq    -> text "!="
-- Sets
    F_union  -> text "union"
    F_inter  -> text "inter"
    F_diff   -> text "diff"
    F_Union  -> text "Union"
    F_Inter  -> text "Inter"
    F_member -> text "member"
    F_card   -> text "card"
    F_empty  -> text "empty"
    F_set    -> text "set"
    F_Set    -> text "Set"
    F_Seq    -> text "Seq"
-- Types
    F_Int    -> text "Int"
    F_Bool   -> text "Bool"
--Sequences
    F_null   -> text "null"
    F_head   -> text "head"
    F_tail   -> text "tail"
    F_concat -> text "concat"
    F_elem   -> text "elem"
    F_length -> text "length"
    F_Concat -> text "^"
    F_Len2   -> text "#"
--process oprators
    F_STOP   -> text "STOP"
    F_SKIP   -> text "SKIP"
    F_Events -> text "Events"
    F_CHAOS  -> text "CHAOS"
    F_Guard  -> text "&"
    F_Sequential -> text ";"
    F_Interrupt  -> text "/\\"
    F_ExtChoice  -> text "[]"
    F_IntChoice  -> text "|~|"
    F_Hiding     -> text "\\"
    F_Timeout    -> text "[>"
    F_Interleave -> text "|||"
