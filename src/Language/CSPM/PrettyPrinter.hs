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

module Language.CSPM.PrettyPrinter
(
 pp
,prettyPrintFile
)
where

import Text.PrettyPrint

import Language.CSPM.AST
import Language.CSPM.Utils(parseFile)

-- | run the pretty printer on a file and write the output to
-- | filename.ast and filename.pretty
prettyPrintFile :: FilePath -> IO ()
prettyPrintFile f = do
  ast      <- parseFile f
  writeFile (f ++ ".ast") (show ast)
  writeFile (f ++ ".pretty") $ render $ pp ast

class PP x where
  pp :: x -> Doc

instance (PP x) => PP (Labeled x) where
  pp = pp . unLabel

instance PP (Module a) where
  pp m = vcat $ map pp (moduleDecls m)

-- help functions for the Instances of the Type-class PP
dot :: Doc
dot = text "."

ppListSet :: (PP r) => String -> String -> r -> Maybe [LCompGen] -> Doc
ppListSet str1 str2 range mgen  = 
     case mgen of
       Nothing  -> text str1 <+>  pp range <+>  text str2
       Just gen -> text str1 <+> pp range <+> text "|" <+> (hsep $ punctuate comma (map (ppCompGen False) gen)) <+> text str2 

hsepPunctuate :: (PP t) => Doc -> [t] -> Doc
hsepPunctuate s l = hsep $ punctuate s $ map pp l

hcatPunctuate :: (PP t) => Doc -> [t] -> Doc
hcatPunctuate s l = hcat $ punctuate s $ map pp l

printFunBind :: LIdent -> [FunCase] -> Doc
printFunBind ident lcase = vcat $ map (printIdent (unLabel ident) <>) (map printCase lcase) 

printCase :: FunCase -> Doc
printCase c = case c of
  FunCaseI pat  expr -> (parens $ hcatPunctuate comma pat) <+> equals <+> pp expr
  FunCase  list expr -> (hcat $ map mkPat list) <+> equals <+> pp expr
  where
    mkPat l = parens $ hcatPunctuate comma l

instance PP Decl where
  pp d = case d of
    PatBind pat expr -> pp pat  <+> equals <+> (pp expr)
    FunBind ident lcase -> printFunBind ident lcase
    Assert a -> text "assert" <+> pp a
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
    Print expr -> text "print"  <+> pp expr

printFunArgs :: [[LExp]] -> Doc
printFunArgs = hcat . map (parens . hsepPunctuate comma)

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

instance PP Exp where
  pp expression = case expression of
    Var ident -> printIdent $ unLabel ident
    IntExp i -> integer i
    SetExp range mgen -> ppListSet "{"  "}" range mgen
    ListExp range mgen -> ppListSet "<"  ">" range mgen
    ClosureComprehension (lexp,lcomp)
      -> ppListSet "{|" "|}" (labeled $ RangeEnum lexp) (Just lcomp)
    Let ldecl expr -> vcat
      [
       nest 2 (text "let")
      ,vcat $ punctuate (text "" $$ nest 4 (text "")) (map pp ldecl)
      ,nest 2 (text "within" <+> pp expr)
      ]
    Ifte expr1 expr2 expr3 -> vcat
      [
       nest 2 $ text "if" <+> pp expr1
      ,nest 4 $ text "then" <+> pp expr2
      ,nest 4 $ text "else" <+> pp expr3
      ]
    CallFunction expr list -> pp expr  <> printFunArgs list
    CallBuiltIn  builtin [expr] -> pp builtin <> (parens $ hsepPunctuate comma expr)
    CallBuiltIn  builtin _ -> error "pp Exp: builtin must exactly have one argument"
    Lambda pat expr -> text "\\" <+> hsepPunctuate comma pat <+> text "@" <+> pp expr
    Stop    -> text "STOP"
    Skip    -> text "SKIP"
    CTrue   -> text "true"
    CFalse  -> text "false"
    Events  -> text "Events"
    BoolSet -> text "Bool"
    IntSet  -> text "Int"
    ProcSet -> text "Proc"
    TupleExp e      -> parens $ hsepPunctuate comma e
    Parens e        -> parens $ pp e
    AndExp a b      -> pp a <+> text "and" <+> pp b
    OrExp a b       -> pp a<+> text "or" <+> pp b
    NotExp e        -> text "not" <+> pp e
    NegExp e        -> text " " <> text "-" <>  pp e
    Fun1 builtin e  -> pp builtin <> pp e
    Fun2 builtin a b -> pp a <+> pp builtin <+> pp b
    DotTuple l      -> hcatPunctuate dot l
    Closure e       -> text "{|" <+> hsepPunctuate comma e <+> text "|}"

-- process expressions
    ProcSharing e p1 p2 -> pp p1 <> text "[|" <+> pp e <+> text "|]" <> pp p2
    ProcAParallel expr1 expr2  p1 p2
      -> pp p1 <> (brackets $ pp expr1 <+> text "||" <+> pp expr2) <> pp p2
    ProcLinkParallel llist p1 p2 -> pp p1 <> pp llist <> pp p2
    ProcRenaming renames mgen proc
      -> pp proc  <> text "[[" <+> hsepPunctuate comma renames <+> gens <+> text "]]"
         where
           gens = case mgen of
                    Nothing   -> empty
                    Just lgen -> text "|" <+> (separateGen False (unLabel lgen))
                                                                          
    ProcException e p1 p2 -> pp p1 <+> text "[|" <+> pp e <+> text "|>" <+> pp p2
    ProcRepSequence lgen proc -> replicatedProc (text ";")   (unLabel lgen) proc
    ProcRepInternalChoice lgen proc -> replicatedProc (text "|~|") (unLabel lgen) proc
    ProcRepExternalChoice lgen proc -> replicatedProc (text "[]")  (unLabel lgen) proc
    ProcRepInterleave lgen proc -> replicatedProc (text "|||") (unLabel lgen) proc
    PrefixExp expr fields proc
        -> pp expr <> (hcat $ map pp fields) <+> text "->" <+> pp proc
    ProcRepSharing lgen expr proc
        ->     text "[|" <+> pp expr <+> text "|]" 
           <+> (separateGen True (unLabel lgen)) <+> text "@" <+> pp proc
    ProcRepAParallel lgen expr proc
        -> text "||" <+> (separateGen True (unLabel lgen)) <+> text "@" 
                                                  <+> (brackets $ pp expr) <+> pp proc
    ProcRepLinkParallel lgen llist proc
        -> pp llist  <+> (separateGen True (unLabel lgen)) <+> text "@" <+> pp proc

-- only used in later stages
-- this do not affect the CSPM notation: same outputs as above
    PrefixI _ expr fields proc -> pp expr <> (hcat $ map pp fields) <+> text "->" <+> pp proc
    LetI decls _ expr -> hcat
       [
        nest 2 $ text "let"
       ,nest 4 $ hcat $ map pp decls
       ,nest 2 $ text "within" <+> pp expr
       ]
    LambdaI _ pat expr
        -> text "\\" <+> hsepPunctuate comma pat <+> text "@" <+> pp expr
    ExprWithFreeNames _ expr -> pp expr

replicatedProc :: Doc -> [LCompGen] -> LProc -> Doc
replicatedProc op lgen proc = op <+> (separateGen True lgen) <+> text "@" <+> pp proc

instance PP LinkList where
  pp (LinkList list)                   = brackets $ hsepPunctuate comma list
  pp (LinkListComprehension lgen list)
    = brackets (hsepPunctuate comma list <+> text "|" <+> separateGen False lgen)

instance PP Link where
  pp (Link expr1 expr2) = pp expr1 <+> text "<->" <+> pp expr2

instance PP Rename where
  pp (Rename expr1 expr2) = pp expr1 <+> text "<-" <+> pp expr2

separateGen :: Bool -> [LCompGen] -> Doc
separateGen b lgen = hsep $ punctuate comma $ map (ppCompGen b) lgen 

-- the generators of the comprehension sets, lists (all after the |) and 
-- inside replicated processes (like "x: {1..10}", in this case the bool variable must be true,
-- otherwise false)
ppCompGen :: Bool -> LCompGen -> Doc 
ppCompGen b gen = case unLabel gen of 
  (Generator pat expr) -> (pp pat) <+> case b of
           False -> text "<-" <+> (pp expr) 
           True  -> text ":"  <+> (pp expr)
  (Guard expr)         -> pp expr

-- the range of sets and lists
instance PP Range where
  pp r = case r of
    RangeEnum expr -> hsepPunctuate comma expr
    RangeClosed a b -> pp a <> text ".." <> pp b
    RangeOpen expr -> pp expr <> text ".."

-- unwrapp the BuiltIn-oparator
instance PP BuiltIn where
  pp (BuiltIn c) = pp c

-- the communication fields
instance PP CommField where
  pp (InComm pat)            = text "?" <> pp pat
  pp (InCommGuarded pat expr) = text "?" <> pp pat <> text ":" <> pp expr
  pp (OutComm expr)           = text "!" <> pp expr

-- pretty-printing for CSPM-Patterns
instance PP Pattern where
  pp pattern = case pattern of
    IntPat n         -> integer n
    TruePat          -> text "true"
    FalsePat         -> text "false"
    WildCard         -> text "_"
    ConstrPat ident  -> printIdent $ unLabel ident
    Also pat         -> ppAlso (Also pat)
    Append pat       -> hcatPunctuate (text "^") pat
    DotPat []        -> error "pp Pattern: empty dot pattern"
    DotPat [pat]     -> pp pat
    DotPat l         -> hcat $ punctuate dot $ map nestedDotPat l
    SingleSetPat pat -> text "{" <+> (pp pat) <+> text "}"
    EmptySetPat      -> text "{ }"
    ListEnumPat pat  -> text "<" <+> hsepPunctuate comma pat <+> text ">"
    TuplePat pat     -> text "(" <> hsepPunctuate comma pat <>  text ")"
    VarPat ident     -> printIdent $ unLabel ident
    Selectors _ _    -> error "pp Pattern Seclectors"
    Selector _ _     -> error "pp Pattern Seclector"
    where
      nestedDotPat p = case unLabel p of
        DotPat {} ->parens $ pp p
        x -> pp x

-- external function for Also-Patterns for a better look
ppAlso :: Pattern -> Doc
ppAlso (Also [])    = text ""
ppAlso (Also (h:t)) = 
   case unLabel h of
     DotPat _ -> if length t > 0 then (pp h) <> text "@@" <> ppAlso (Also t)
                                 else pp h
     Append _ -> if length t > 0 then (pp h) <> text "@@" <> ppAlso (Also t)
                                 else pp h
     _        -> if length t > 0 then pp h <> text "@@" <> ppAlso (Also t)
                                 else pp h
ppAlso _ = text ""

-- disticts the cases for different syntax-records for the Ident datatype
printIdent :: Ident -> Doc
printIdent ident = 
  case ident of 
   Ident _  -> text $ unIdent ident
   UIdent _ -> text $ realName $ unUIdent ident 

instance PP AssertDecl where
  pp a = case a of
     AssertBool expr -> pp expr
     AssertRefine n expr1 op expr2
       -> negated n $ pp expr1 <+> pp op <+> pp expr2
     AssertTauPrio n expr1 op expr2 expr3
       -> negated n $ pp expr1 <+> pp op <+> pp expr2 <+> text ":[tau priority over]:" <+> pp expr3
     AssertModelCheck n expr m mb
       -> negated n $ pp expr <+> text ":[" <+> pp m <+> maybe empty pp mb <+> text "]"
    where
      negated ar doc = if ar then text "not" <+> doc else doc

instance PP FdrExt where
  pp F  = text "[F]"
  pp FD = text "[FD]"
  pp T  = text "[T]"

instance PP FDRModels where
  pp DeadlockFree  = text "deadlock free"
  pp Deterministic = text "deterministic"
  pp LivelockFree  = text "livelock free"

instance PP RefineOp where 
  pp Trace = text "[T="
  pp Failure = text "[F="
  pp FailureDivergence = text "[FD="
  pp RefusalTesting = text "[R="
  pp RefusalTestingDiv = text "[RD="
  pp RevivalTesting = text "[V="
  pp RevivalTestingDiv = text "[VD="
  pp TauPriorityOp = text "[TP="

instance PP TauRefineOp where
  pp TauTrace = text "[T="
  pp TauRefine = text "[="

instance PP Const where
-- Booleans
  pp F_true   = text "true"
  pp F_false  = text "false"
  pp F_not    = text "not"
  pp F_and    = text "and"
  pp F_or     = text "or"
-- Numbers
  pp F_Mult   = text "*"
  pp F_Div    = text "/"
  pp F_Mod    = text "%"
  pp F_Add    = text "+"
  pp F_Sub    = text "" <+> text "-"
-- Equality
  pp F_GE     = text ">="
  pp F_LE     = text "<="
  pp F_LT     = text "<"
  pp F_GT     = text ">"
  pp F_Eq     = text "=="
  pp F_NEq    = text "!="
-- Sets
  pp F_union  = text "union"
  pp F_inter  = text "inter"
  pp F_diff   = text "diff"
  pp F_Union  = text "Union"
  pp F_Inter  = text "Inter"
  pp F_member = text "member"
  pp F_card   = text "card"
  pp F_empty  = text "empty"
  pp F_set    = text "set"
  pp F_Set    = text "Set"
  pp F_Seq    = text "Seq"
-- Types
  pp F_Int    = text "Int"
  pp F_Bool   = text "Bool"
--Sequences
  pp F_null   = text "null"
  pp F_head   = text "head"
  pp F_tail   = text "tail"
  pp F_concat = text "concat" 
  pp F_elem   = text "elem"
  pp F_length = text "length"
  pp F_Concat = text "^" 
  pp F_Len2   = text "#"
--process oprators
  pp F_STOP   = text "STOP"
  pp F_SKIP   = text "SKIP"
  pp F_Events = text "Events"
  pp F_CHAOS  = text "CHAOS"
  pp F_Guard  = text "&"
  pp F_Sequential = text ";"
  pp F_Interrupt  = text "/\\"
  pp F_ExtChoice  = text "[]"
  pp F_IntChoice  = text "|~|"
  pp F_Hiding     = text "\\"
  pp F_Timeout    = text "[>"
  pp F_Interleave = text "|||"
