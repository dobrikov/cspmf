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
-- Abstract Syntax Tree to CSPM syntax.
-----------------------------------------------------------------------------

module PrettyPrinterNew
where

import Text.PrettyPrint
import Data.Maybe

import Language.CSPM.AST -- the module where the syntax for the AST defined is
import Language.CSPM.Utils(parseFile) -- we use the parseFile function here for parsing of the generated cspm code

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

dot :: Doc
dot = text "."

ppListSet :: (PP r) => String -> String -> r -> Maybe [LCompGen] -> Doc
ppListSet str1 str2 range mgen  = 
     case mgen of
       Nothing  -> text str1 <>  pp range <>  text str2
       Just gen -> text str1 <+> pp range <+> text "|" <+> (hsep $ punctuate comma (map (ppCompGen False) gen)) <+> text str2 

separateElemsWith :: (PP t) => [t] -> Doc -> Doc
separateElemsWith list sep = hsep $ punctuate sep (map pp list)

catElemsWith :: (PP t) => [t] -> Doc -> Doc
catElemsWith list sep = hcat $ punctuate sep $ map pp list

printFunBind :: LIdent -> [FunCase] -> Doc
printFunBind ident lcase = vcat $ map (printIdent (unLabel ident) <>) (map printCase lcase) 

printCase :: FunCase -> Doc
printCase c = 
   case c of 
    FunCaseI pat  exp -> ((parens $ catElemsWith pat comma) <+> equals <+> pp exp)
    FunCase [pat] exp -> ((parens $ catElemsWith pat comma) <+> equals <+> pp exp) 

instance PP Decl where
  pp (PatBind     pat    exp)         = pp pat  <+> equals <+> (pp exp)
  pp (FunBind     ident  lcase)       = printFunBind ident lcase
  pp (AssertRef   exp1   s exp2)      = text "assert"      <+> pp exp1 <+> text s <+> pp exp2 
                                       -- <+> case mexp of -- incomplete
                                       --      Nothing     -> empty
                                        --     Just    exp -> text ":" <+> (brackets $ pp exp)
  pp (AssertBool  exp)                = text "assert"      <+> pp exp 
  pp (Transparent idents)             = text "transparent" <+> (hsep $ punctuate comma (map (printIdent . unLabel) idents))
  pp (SubType     ident  constrs)     = text "subtype"     <+> printIdent (unLabel ident) <+> equals 
                                        <+> (vcat $ punctuate (text "|") (map printConstr (map unLabel constrs)))
  pp (DataType    ident  constrs)     = text "datatype"    <+> printIdent (unLabel ident) <+> equals 
                                        <+> (vcat $ punctuate (text "|") (map printConstr (map unLabel constrs)))
  pp (NameType    ident  typ)         = text "nametype"    <+> printIdent (unLabel ident) <+> equals <+> typeDef typ
  pp (Channel     idents typ)         = text "channel"     <+> (hsep $ punctuate comma $ map (printIdent . unLabel) idents) <+>
                                   case typ of
                                     Nothing 
                                      -> empty
                                     Just t 
                                      -> text ":" <+> typeDef t
  pp (Print  exp )                    = text "print"  <+> pp exp

printConstr :: Constructor -> Doc
printConstr (Constructor ident typ) = printIdent (unLabel ident) <>
  case typ of 
   Nothing -> empty
   Just t  -> dot <> typeDef  t

typeDef :: LTypeDef -> Doc
typeDef typ = case unLabel typ of
               TypeTuple lexp ->  parens $ catElemsWith lexp comma
               TypeDot   lexp ->  catElemsWith lexp dot

instance PP Exp where
  pp (Var     ident)                     = printIdent $ unLabel ident
  pp (IntExp  int)                       = integer int
  pp (SetExp  range mgen)                = ppListSet "{"  "}"            range               mgen
  pp (ListExp range mgen)                = ppListSet "<"  ">"            range               mgen
  pp (ClosureComprehension (lexp,lcomp)) = ppListSet "{|" "|}" (labeled $ RangeEnum lexp) (Just lcomp)
  pp (Let     ldecl exp)                 = text "" $$ (nest 2 (text "let"))
                                                                $$ (hcat $ punctuate (text "" $$ nest 4 (text "")) (map pp ldecl)) 
                                                                        $$ (nest 2 (text "within" <+> pp exp))
  pp (Ifte    exp1  exp2 exp3)           = text "" $$ (nest 2 (text "if")) <+> pp exp1
                                                                $$ nest 4 (text "then") <+> pp exp2 
                                                                        $$ nest 4 (text "else" <+> pp exp3)
  pp (CallFunction exp   [lexp])         = pp exp   <> (parens $ separateElemsWith lexp comma)
  pp (CallBuiltIn  built [lexp])         = pp built <> (parens $ separateElemsWith lexp comma)
  pp (Lambda lpat exp)                   = text "\\" <+> (separateElemsWith lpat comma <+> text "@") <+> pp exp
  pp Stop                                = text "STOP"
  pp Skip                                = text "SKIP"
  pp CTrue                               = text "true"
  pp CFalse                              = text "false"
  pp Events                              = text ""
  -- | BoolSet
  -- | IntSet
  -- | ProcSet
  pp (TupleExp lexp)                     = parens $ separateElemsWith lexp comma
  pp (Parens   exp)                      = parens $ pp exp
  pp (AndExp   exp1  exp2)               = pp exp1 <+> text "and" <+> pp exp2
  pp (OrExp    exp1  exp2)               = pp exp1 <+> text "or"  <+> pp exp2
  pp (NotExp   exp)                      = text "not" <+> pp exp
  pp (NegExp   exp)                      = text "-"   <>  pp exp
  pp (Fun1     built exp)                = pp built   <>  pp exp
  pp (Fun2     built exp1 exp2)          = pp exp1    <+> pp built <+> pp exp2
  pp (DotTuple lexp)                     = catElemsWith lexp (dot)
  pp (Closure  lexp)                     = text "{|" <+> separateElemsWith lexp comma <+> text "|}"
-- process expressions
  pp (ProcSharing      exp     proc1 proc2)       = pp proc1 <>       text "[|" <+> pp exp <+> text "|]"       <> pp proc2
  pp (ProcAParallel    exp1    exp2  proc1 proc2) = pp proc1 <> (brackets $ pp exp1 <+> text "||" <+> pp exp2) <> pp proc2
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
  pp (PrefixExp             exp  fields proc)    = pp exp <> (hcat $ map pp fields) <+> text "->" <+> pp proc
  pp (ProcRepSharing        lgen exp    proc)    = text "[|" <+> pp exp <+> text "|]" 
                                                  <+> (separateGen True (unLabel lgen)) <+> text "@" <+> pp proc
  pp (ProcRepAParallel      lgen exp    proc)    = text "||" <+> (separateGen True (unLabel lgen)) <+> text "@" 
                                                  <+> (brackets $ pp exp) <+> pp proc
  pp (ProcRepLinkParallel   lgen llist  proc)    = pp llist  <+> (separateGen True (unLabel lgen)) <+> text "@"
                                                  <+> pp proc
-- only used in later stages
-- this do not affect the CSPM notation: same outputs as above
  pp (PrefixI _ exp fields proc)                 = pp exp <> (hcat $ map pp fields) <+> text "->" <+> pp proc
  pp (LetI    ldecl _      exp)                  = text "" $$ (nest 2 (text "let"))
                                                                $$ (hcat $ punctuate (text "" $$ nest 4 (text "")) (map pp ldecl)) 
                                                                        $$ (nest 2 (text "within" <+> pp exp))
  pp (LambdaI  _     lpat exp)                   = text "\\" <+> (separateElemsWith lpat comma <+> text "@") <+> pp exp
  pp (ExprWithFreeNames _ exp)                 = pp exp

replicatedProc :: Doc -> [LCompGen] -> LProc -> Doc
replicatedProc op lgen proc = op <+> (separateGen True lgen) <+> text "@" <+> pp proc

instance PP LinkList where
  pp (LinkList list)                   = brackets $ separateElemsWith list comma
  pp (LinkListComprehension lgen list) = brackets  (separateElemsWith list comma <+> text "|" <+>  separateGen False lgen)

instance PP Link where
  pp (Link exp1 exp2) = pp exp1 <+> text "<->" <+> pp exp2

instance PP Rename where
  pp (Rename exp1 exp2) = pp exp1 <+> text "<-" <+> pp exp2

separateGen :: Bool -> [LCompGen] -> Doc
separateGen b lgen = hsep $ punctuate comma $ map (ppCompGen b) lgen 

-- the generators of the comprehension sets, lists (all after the |) and 
-- inside replicated processes (like "x: {1..10}", in this case the bool variable must be true,
-- otherwise false)
ppCompGen :: Bool -> LCompGen -> Doc 
ppCompGen b gen = case unLabel gen of 
  (Generator pat exp) -> (pp pat) <+> case b of
           False -> text "<-" <+> (pp exp) 
           True  -> text ":"  <+> (pp exp)
  (Guard exp)         -> pp exp

-- the range of sets and lists
instance PP Range where
  pp (RangeEnum lexp)        = separateElemsWith lexp comma
  pp (RangeClosed exp1 exp2) = (pp exp1) <> text ".." <> (pp exp2)
  pp (RangeOpen exp)         = (pp exp) <> text ".."

-- unwrapp the BuiltIn-oparator
instance PP BuiltIn where
  pp (BuiltIn c) = pp c

-- the communication fields
instance PP CommField where
  pp (InComm pat)            = text "?" <> pp pat
  pp (InCommGuarded pat exp) = text "?" <> pp pat <> text ":" <> pp exp
  pp (OutComm exp)           = text "!" <> pp exp

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
  pp F_Sub    = text "-"
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
