----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.PrettyPrinterNew
-- Copyright   :  (c) Ivaylo Dobrikov 2010
-- License     :  BSD
-- 
-- Maintainer  :  me@dobrikov.biz
-- Stability   :  experimental
-- Portability :  GHC-only
-- 
-- This module defines functions for pretty-printing the 
-- Abstract Syntax Tree to CSPM syntax. Support Instances and functions 
-- of all Typescases except the constructors AssertRef (in Decl), ProcSet (in Exp) and ProcException (in Exp)
-- TODO: better output view for the printed tree. Make printer to pretty printer :).
-----------------------------------------------------------------------------

module Language.CSPM.PrettyPrinter
where

import Text.PrettyPrint
--import Data.Maybe

import Language.CSPM.AST -- the module where the syntax for the AST defined is
import Language.CSPM.Utils(parseFile) -- we use the parseFile function here for parsing of the generated cspm code
import Language.CSPM.SrcLoc

-- give just the file name back
dropCsp :: String -> String
dropCsp str = fst $ break (== '.') str

runPretty :: FilePath -> IO ()
runPretty f = 
 do 
  str      <- parseFile f
  let fileName = dropCsp f
  writeFile (fileName ++ ".ast") (show str)
  writeFile (fileName ++ "Pretty.csp") (toPrettyString str) 

toPrettyString :: LModule -> String
toPrettyString lmodule = render $ pp lmodule

class PP x where
  pp :: x -> Doc

instance (PP x) => PP (Labeled x) where
  pp = pp . unLabel

instance PP Module where
  pp m = vcat $ map pp (moduleDecls m)

-- help functions for the Instances of the Type-class PP
dot :: Doc
dot = text "."

ppListSet :: (PP r) => String -> String -> r -> Maybe [LCompGen] -> Doc
ppListSet str1 str2 range mgen  = 
     case mgen of
       Nothing  -> text str1 <+>  pp range <+>  text str2
       Just gen -> text str1 <+> pp range <+> text "|" <+> (hsep $ punctuate comma (map (ppCompGen False) gen)) <+> text str2 

separateElemsWith :: (PP t) => [t] -> Doc -> Doc
separateElemsWith list s = hsep $ punctuate s (map pp list)

catElemsWith :: (PP t) => [t] -> Doc -> Doc
catElemsWith list s = hcat $ punctuate s $ map pp list

printFunBind :: LIdent -> [FunCase] -> Doc
printFunBind ident lcase = vcat $ map (printIdent (unLabel ident) <>) (map printCase lcase) 

printCase :: FunCase -> Doc
printCase c = 
   case c of 
    FunCaseI pat  expr -> ((parens $ catElemsWith pat comma) <+> equals <+> pp expr)
    FunCase (pat:_) expr -> ((parens $ catElemsWith pat comma) <+> equals <+> pp expr) 

instance PP Decl where
  pp (PatBind     pat    expr)         = pp pat  <+> equals <+> (pp expr)
  pp (FunBind     ident  lcase)       = printFunBind ident lcase
  pp (Assert assertexpr)     = text "assert" <+> pp assertexpr  
  pp (Transparent ids)             = text "transparent" <+> (hsep $ punctuate comma (map (printIdent . unLabel) ids))
  pp (SubType     ident  constrs)     = text "subtype"     <+> printIdent (unLabel ident) <+> equals 
                                        <+> (vcat $ punctuate (text "|") (map printConstr (map unLabel constrs)))
  pp (DataType    ident  constrs)     = text "datatype"    <+> printIdent (unLabel ident) <+> equals 
                                        <+> (hsep $ punctuate (text "|") (map printConstr (map unLabel constrs)))
  pp (NameType    ident  typ)         = text "nametype"    <+> printIdent (unLabel ident) <+> equals <+> typeDef typ
  pp (Channel     ids typ)         = text "channel"     <+> (hsep $ punctuate comma $ map (printIdent . unLabel) ids) <+>
                                   case typ of
                                     Nothing 
                                      -> empty
                                     Just t 
                                      -> text ":" <+> typeDef t
  pp (Print  expr )                    = text "print"  <+> pp expr

-- Contructors
printConstr :: Constructor -> Doc
printConstr (Constructor ident typ) = printIdent (unLabel ident) <>
  case typ of 
   Nothing -> empty
   Just t  -> dot <> typeDef  t

-- Type Definitions
typeDef :: LTypeDef -> Doc
typeDef typ = case unLabel typ of
               TypeTuple lexp ->  parens $ catElemsWith lexp comma
               TypeDot   lexp ->  catElemsWith lexp dot

instance PP Exp where
  pp (Var     ident)                     = printIdent $ unLabel ident
  pp (IntExp  i)                         = integer i
  pp (SetExp  range mgen)                = ppListSet "{"  "}"            range               mgen
  pp (ListExp range mgen)                = ppListSet "<"  ">"            range               mgen
  pp (ClosureComprehension (lexp,lcomp)) = ppListSet "{|" "|}" (labeled $ RangeEnum lexp) (Just lcomp)
  pp (Let     ldecl expr)                 = text "" $$ (nest 2 (text "let"))
                                                                $$ (vcat $ punctuate (text "" $$ nest 4 (text "")) (map pp ldecl)) 
                                                                        $$ (nest 2 (text "within" <+> pp expr))
  pp (Ifte    expr1  expr2 expr3)           = text "" $$ (nest 2 (text "if")) <+> pp expr1
                                                                $$ nest 4 (text "then") <+> pp expr2 
                                                                        $$ nest 4 (text "else" <+> pp expr3)
  pp (CallFunction expr   [lexp])         = pp expr   <> (parens $ separateElemsWith lexp comma)
  pp (CallBuiltIn  built [lexp])         = pp built <> (parens $ separateElemsWith lexp comma)
  pp (Lambda lpat expr)                   = text "\\" <+> (separateElemsWith lpat comma <+> text "@") <+> pp expr
  pp Stop                                = text "STOP"
  pp Skip                                = text "SKIP"
  pp CTrue                               = text "true"
  pp CFalse                              = text "false"
  pp Events                              = text "Events"
  pp BoolSet                             = text "Bool"
  pp IntSet                              = text "Int"
--  pp ProcSet 
  pp (TupleExp lexp)                     = parens $ separateElemsWith lexp comma
  pp (Parens   expr)                      = parens $ pp expr
  pp (AndExp   expr1  expr2)               = pp expr1 <+> text "and" <+> pp expr2
  pp (OrExp    expr1  expr2)               = pp expr1 <+> text "or"  <+> pp expr2
  pp (NotExp   expr)                      = text "not" <+> pp expr
  pp (NegExp   expr)                      = text " " <+> text "-"   <>  pp expr
  pp (Fun1     built expr)                = pp built   <>  pp expr
  pp (Fun2     built expr1 expr2)          = pp expr1    <+> pp built <+> pp expr2
  pp (DotTuple lexp)                     = catElemsWith lexp (dot)
  pp (Closure  lexp)                     = text "{|" <+> separateElemsWith lexp comma <+> text "|}"
-- process expressions
  pp (ProcSharing      expr     proc1 proc2)       = pp proc1 <>       text "[|" <+> pp expr <+> text "|]"       <> pp proc2
  pp (ProcAParallel    expr1    expr2  proc1 proc2) = pp proc1 <> (brackets $ pp expr1 <+> text "||" <+> pp expr2) <> pp proc2
  pp (ProcLinkParallel llist   proc1 proc2)       = pp proc1 <>                   pp llist                     <> pp proc2
  pp (ProcRenaming     lrename mgen  proc )       = pp proc  <> text "[[" <+> case mgen of
                                                                                 Nothing   -> separateElemsWith lrename comma
                                                                                 Just lgen -> (separateElemsWith lrename comma) 
                                                                                                <+> text "|" <+> (separateGen False (unLabel lgen))
                                                                          <+> text "]]"
--  pp (ProcException LProc LExp LProc) -- ask Mark...
--  | ProcRenamingComprehension [LRename] [LCompGen] LProc ==== does not exist in the CSPM Notation
  pp (ProcRepSequence       lgen proc)           = replicatedProc (text ";")   (unLabel lgen) proc
  pp (ProcRepInternalChoice lgen proc)           = replicatedProc (text "|~|") (unLabel lgen) proc
  pp (ProcRepExternalChoice lgen proc)           = replicatedProc (text "[]")  (unLabel lgen) proc
  pp (ProcRepInterleave     lgen proc)           = replicatedProc (text "|||") (unLabel lgen) proc
  pp (PrefixExp             expr  fields proc)    = pp expr <> (hcat $ map pp fields) <+> text "->" <+> pp proc
  pp (ProcRepSharing        lgen expr    proc)    = text "[|" <+> pp expr <+> text "|]" 
                                                  <+> (separateGen True (unLabel lgen)) <+> text "@" <+> pp proc
  pp (ProcRepAParallel      lgen expr    proc)    = text "||" <+> (separateGen True (unLabel lgen)) <+> text "@" 
                                                  <+> (brackets $ pp expr) <+> pp proc
  pp (ProcRepLinkParallel   lgen llist  proc)    = pp llist  <+> (separateGen True (unLabel lgen)) <+> text "@"
                                                  <+> pp proc
-- only used in later stages
-- this do not affect the CSPM notation: same outputs as above
  pp (PrefixI _ expr fields proc)                 = pp expr <> (hcat $ map pp fields) <+> text "->" <+> pp proc
  pp (LetI    ldecl _      expr)                  = text "" $$ (nest 2 (text "let"))
                                                                $$ (hcat $ punctuate (text "" $$ nest 4 (text "")) (map pp ldecl)) 
                                                                        $$ (nest 2 (text "within" <+> pp expr))
  pp (LambdaI  _     lpat expr)                   = text "\\" <+> (separateElemsWith lpat comma <+> text "@") <+> pp expr
  pp (ExprWithFreeNames _ expr)                 = pp expr

replicatedProc :: Doc -> [LCompGen] -> LProc -> Doc
replicatedProc op lgen proc = op <+> (separateGen True lgen) <+> text "@" <+> pp proc

instance PP LinkList where
  pp (LinkList list)                   = brackets $ separateElemsWith list comma
  pp (LinkListComprehension lgen list) = brackets  (separateElemsWith list comma <+> text "|" <+>  separateGen False lgen)

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
  pp (RangeEnum lexp)        = separateElemsWith lexp comma
  pp (RangeClosed expr1 expr2) = (pp expr1) <> text ".." <> (pp expr2)
  pp (RangeOpen expr)         = (pp expr) <> text ".."

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
  pp (IntPat n)         = integer n
  pp TruePat            = text "true"
  pp FalsePat           = text "false"  
  pp WildCard           = text "_"
  pp (ConstrPat ident)  = printIdent $ unLabel ident
  pp (Also pat)         = ppAlso (Also pat)
  pp (Append pat)       = catElemsWith pat (text "^")
  pp (DotPat pat)       = catElemsWith pat (dot)
  pp (SingleSetPat pat) = text "{" <+> (pp pat) <+> text "}"  
  pp EmptySetPat        = text "{ }"
  pp (ListEnumPat pat)  = text "<" <+>  separateElemsWith pat comma <+> text ">"
  pp (TuplePat pat)     = text "(" <>   separateElemsWith pat comma <>  text ")"
  pp (VarPat ident)     = printIdent $ unLabel ident
  pp (Selectors _ _)    = empty
  pp (Selector _ _)       = empty

-- external function for Also-Patterns for a better look
ppAlso :: Pattern -> Doc
ppAlso (Also [])    = text ""
ppAlso (Also (h:t)) = 
   case unLabel h of
     DotPat _ -> text "(" <> (pp h) <> text ")" <> text "@@" <> ppAlso (Also t)
     Append _ -> text "(" <> (pp h) <> text ")" <> text "@@" <> ppAlso (Also t)
     _        -> pp h <> text "@@" <> ppAlso (Also t)
ppAlso _ = text ""

-- disticts the cases for different syntax-records for the Ident datatype
printIdent :: Ident -> Doc
printIdent ident = 
  case ident of 
   Ident _  -> text $ unIdent ident
   UIdent _ -> text $ realName $ unUIdent ident 

instance PP AssertExp where
  pp (AssertBool expr) = pp expr
  pp (AssertListRef expr1 op expr2) = pp expr1 <+> pp op <+> pp expr2
  pp (AssertTauPrio expr1 op expr2 expr3) = pp expr1 <+> pp op <+> pp expr2 <+> text ":[tau priority over]:" <+> pp expr3
  pp (AssertInternalFDRChecks expr m) = pp expr <+> pp m

instance PP FDRModels where
  pp DeadlockFreeF = text ":[ deadlock free [F] ]"
  pp DeadlockFreeFD = text ":[ deadlock free [FD] ]"
  pp DeterministicFreeF = text ":[ deterministic free [F] ]"
  pp DeterministicFreeFD = text ":[ deterministic free [FD] ]"
  pp LivelockFree = text ":[ divergence free ]"

instance PP AssertOp where 
  pp F_Trace = text "[T="
  pp F_Failure = text "[F="
  pp F_FailureDivergence = text "[FD="
  pp F_RefusalTesting = text "[R="
  pp F_RefusalTestingDiv = text "[RD="
  pp F_RevivalTesting = text "[V="
  pp F_RevivalTestingDiv = text "[VD="
  pp F_TauPriorityOp = text "[TP="

instance PP AssertTOp where
  pp F_TTrace = text "[T="
  pp F_Refine = text "[="

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
