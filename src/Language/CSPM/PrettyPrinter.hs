{-# OPTIONS_GHC
    -XTypeSynonymInstances #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.PrettyPrinter
-- Copyright   :  (c) Dobrikov 2008
-- License     :  BSD
-- 
-- Maintainer  :  me@dobrikov.biz
-- Stability   :  experimental
-- Portability :  GHC-only
--
module {-Language.CSPM.-}Language.CSPM.PrettyPrinter
where

import Text.PrettyPrint as PrettyPrint hiding (char)
import qualified Text.PrettyPrint as PrettyPrint
import Language.CSPM.AST
import Language.CSPM.Frontend

import Data.Maybe


class PP x where pp :: x-> Doc

instance (PP x) => PP (Labeled x) where 
   pp = pp . unLabel

instance PP Ident where pp = text . unIdent

--instance PP BuiltIn where 
--  pp (BuiltIn fun) =  text $ show fun   

instance PP Exp where 
     pp (Var x) = pp x
     pp (IntExp x) = integer x
     pp (SetEnum x) = lbrace <> (hcat $ punctuate (comma <+> empty) $ map pp x) <> rbrace 
     pp (ListEnum x) = text "<" <> (hcat $ punctuate (comma <+> empty) $ map pp x) <> text ">"
     pp (SetOpen x) = lbrace <> pp x <> text ".." <> rbrace
     pp (ListOpen x) = text "<" <> pp x <> text ".." <> text ">"
     pp (SetComprehension (x{-[LExp]-},l{-[LCompGen]-})) = lbrace <+> (hcat $ punctuate (comma) $ map pp x) <+> text "|" <+> (hcat $ punctuate (comma) $ map pp l) <+> rbrace
     pp (ListComprehension (x,l)) = text "<" <+> (hcat $ punctuate (comma) $ map pp x) <+> text "|" <+> (hcat $ punctuate (comma) $ map pp l) <+> text ">"
     pp (ClosureComprehension (x,l)) = text "{|" <+> (hcat $ punctuate (comma) $ map pp x) <+> text "|" <+> (hcat $ punctuate (comma) $ map pp l) <+> text "|}"
     pp (SetClose (x,y)) = lbrace <> pp x <> text ".." <> pp y <> rbrace
     pp (ListClose (x,y)) = text "<" <> pp x <> text ".." <> pp y <> text ">"
     pp (Parens x) = lparen <> pp x <> rparen
     pp BoolSet = text "Bool"
     pp IntSet = text "Int"
     pp Events = text "Events"
     pp Stop = text "STOP"
     pp Skip = text "SKIP"
     pp CTrue = text "true"
     pp CFalse = text "false"
     pp (TupleExp xlist) = lparen <> (vcat $ punctuate (comma) (map pp xlist)) <> rparen
     pp (DotTuple x) = hcat $ punctuate (text ".") $ map pp x
     pp (Closure x) = text "{|" <+> (hcat $ punctuate (comma <+> empty) $ map pp x) <+> text "|}"
     pp (CallBuiltIn x [list]) = pp x <> lparen <> (hcat $ punctuate (comma) $ map pp list) <> rparen
     pp (Ifte x y z) = text "if" <+> pp x <+> text "then" $$ pp y $$ text "else" <+> pp z
     pp (AndExp x y) = pp x <+> text "and" <+> pp y
     pp (OrExp x y) = pp x <+> text "or" <+> pp y
     pp (NotExp x) = text "not" <+> pp x
     pp (Fun1 x y) = pp x <> pp y
     pp (Fun2 fun x y) =  pp x <+> pp fun <+> pp y
     pp (Lambda patt x) = text "\\" <+> (hcat $ punctuate (comma <+> empty) $ map pp patt) <+> text "@" <+> pp x
     pp (PrefixExp x l_comm_field y) = pp x <> (hcat $ map pp l_comm_field) <+> text "->" <+> pp y 
     pp (ProcSharing x_middle x_left x_right) = pp x_left <+> text "[|" <> pp x_middle <+> text "|]" <> pp x_right
     pp (Let decl x) = text "let" $$ (vcat $ map pp decl) $$ text "within" <+> pp x
     pp (CallFunction x [list]) = pp x <> lparen <> (hcat $ punctuate (comma <+> empty) $ map pp list) <> rparen
     pp (ProcLinkParallel list x y) = pp x <+> pp list <+> pp y
     pp (ProcAParallel x1 x2 x3 x4) = pp x3 <+> lbrack <+> pp x1 <+> text "||" <+> pp x2 <+> rbrack <+> pp x4
--     pp (ProcRepSharing list x x_proc) = pp x <> text "[|" <> comp_gen_list list <> text "|]" <> pp x_proc 
--     pp (ProcRenamingComprehension rlist lcomp proc) = 
--     pp (ProcRepSharing (Labeled s list v) x proc) = 
--     pp (ProcRepInternalChoice list x) = comp_gen_list list <+> text "@" <+> pp x

--instance Rename where
--     pp 

instance PP CompGen where
     pp (Generator patt x) = pp patt <> colon <> pp x
--     pp (Guard x) = 

--data List
--      =  LExp | LCompGen | LDecl | LPattern
--        | LRename | LLink | {-FunCase |-} LConstructor
--        | LIdent 

--instance PP List where
--     pp list = pp list
--fun_punctuate_list :: Doc -> [List] -> [Doc]
--fun_punctuate_list fun list = punctuate fun $ map pp list 

comp_gen_list :: [LCompGen] -> Doc
comp_gen_list l = hcat $ punctuate empty (map pp l)

instance PP BuiltIn where
     pp (BuiltIn const) = pp const  

instance PP LCompGenList where
     pp (Labeled s list v) = (vcat $ punctuate (comma) (map pp list))

--instance PP CompGen where
--     pp (Generator pattern x) = pp x <> colon <> pp pattern
--   pp (Guard x)

instance PP Decl where
     pp (PatBind x y) = pp x <+> equals <+> pp y
     pp (DataType ident list_constr) = text "datatype" <+> pp ident <+> equals <+> (hcat $ punctuate (space <> text "|" <> space) $ map pp list_constr)
     pp (AssertRef x s y) =  text "assert" <+> pp x <+> ptext s <+> pp y 
     pp (AssertBool x) = text "assert" <+> pp x <+> text ":[livelock free]"
     pp (Channel list_x ty_ref) = text "channel" <+> (hcat $ punctuate (comma <> space) $ map pp list_x) <> if isEmpty (pp ty_ref) 
                                                                                                               then empty 
                                                                                                               else space <> colon <> space <> pp ty_ref
     pp (FunBind ident list) = pp ident <> text "(" <> (hcat $ punctuate (comma) $ map pp list)
     pp (SubType ident list_constr) = text "subtype" <+> pp ident <+> equals <+> (hcat $ punctuate (space <> text "|" <> space) $ map pp list_constr)
     pp (NameType ident ty_ref) = text "nametype" <+> pp ident <+> equals <+> pp ty_ref
     pp (Transparent list) = text "transparent" <+> (hcat $ punctuate (comma <> space) $ map pp list)
     pp (Print x) = text "print" <+> pp x

instance PP Module where 
     pp m= vcat $ map pp (moduleDecls m)

instance PP FunCase where
     pp (FunCase [l_fun_args] x) = (hcat $ punctuate (comma <+> empty) $ map pp l_fun_args ) <> text ")" <+> equals <+> pp x 

instance PP LinkList where
     pp (LinkList list_link) = lbrack <> (hcat $ punctuate (comma <+> empty) $ map pp list_link) <> rbrack

instance PP Link where
     pp (Link x y) =  pp x <> text "<->" <> pp y

instance PP Constructor where
     pp (Constructor ident ty_ref) = pp ident <> if isEmpty (pp ty_ref) 
                                                    then empty 
                                                    else text "." <> pp ty_ref

instance (PP x) => PP (Maybe x) where
     pp (Just x) = pp x
     pp Nothing = empty

--instance PP Funcase where -- siehe emptySet
--     pp (Funcase [[arg]] x) = text "(" <>  <> text ")"  <+> equals <+> pp x

instance PP TypeDef where 
--     pp (TypeTuple [x]) = 
     pp (TypeDot x) = (hcat $ punctuate (text ".") $ map pp x)

instance PP Pattern where
     pp (IntPat x) = integer x
     pp (VarPat x) = pp x
     pp EmptySetPat = lbrace <> rbrace 
     pp WildCard = text "_"
     pp (SingleSetPat patt) = lbrace <> pp patt <> rbrace
     pp (ListEnumPat list) = text "<" <> (hcat $ (punctuate (comma) $ map pp list)) <> text ">"

instance PP CommField where
     pp (InComm x) = text "?" <> pp x
     pp (OutComm x) = text "!" <> pp x 
     pp (InCommGuarded pattern x {-LPattern LExp-}) = text "?" <> pp pattern <> colon <> pp x

instance PP Const where
     pp F_true = text "true"
     pp F_false = text "false"
     pp F_not = text "not"
     pp F_and = text "and"
     pp F_or = text "or"
     pp F_STOP = text "STOP"
     pp F_SKIP = text "SKIP"
     pp F_Mult = text "*"
     pp F_Div = colon
     pp F_Add = text "+"
     pp F_Sub = text "-"
     pp F_Eq = text "=="
     pp F_NEq = text "!=" 
     pp F_ExtChoice = text "[]"
     pp F_Union = text "Union"
     pp F_concat = text "concat"
     pp F_Concat = text "^"
     pp F_union = text "union"
     pp F_inter = text "inter"
     pp F_diff = text "diff"
     pp F_Inter = text "Inter"
     pp F_member = text "member"
     pp F_card = text "card"
     pp F_empty = text "empty"
     pp F_set = text "set"
     pp F_Set = text "Set"
     pp F_null = text "null"
     pp F_Seq = text "Seq"
     pp F_head = text "head"
     pp F_tail = text "tail"
     pp F_elem = text "elem"
     pp F_Events = text "Events"
     pp F_Int = text "Int"
     pp F_Bool = text "Bool"
     pp F_GE = text ">="
     pp F_LE = text "<="
     pp F_LT = text "<"
     pp F_GT = text ">"
     pp F_Sequential = text "Sequential"
     pp F_Guard = text "&"
     pp F_Interrupt = text "/\\"
     pp F_Len2 = text "#"
     pp F_CHAOS = text "CHAOS"
     pp F_Timeout = text "[>"
     pp F_IntChoice = text "|~|"
     pp F_Interleave = text "|||"
     pp F_Hiding = text "Hiding"

to_PString :: LModule -> String
to_PString mod = render (pp mod) 


runPretty :: FilePath -> IO String 
runPretty fname = 
  do 
   mod <- parseFile fname
   return (to_PString mod) --(render (pp mod))  

