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
module PrettyPrinter
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
--     pp (SetOpen x) 
--     pp (ListOpen x)
--     pp SetComprehension ([LExp],[LCompGen]) = (hcat $ punctuate (<+>) $ map pp x) <+> text "@" <+> rparen
     pp (SetClose (x,y)) = lbrace <> pp x <> text ".." <> pp y <> rbrace
     pp (ListClose (x,y)) = lbrack <> pp x <> text ".." <> pp y <> rbrack
     pp BoolSet = text "Bool"
     pp IntSet = text "Int"
     pp Events = text "Events"
     pp Stop = text "STOP"
     pp Skip = text "SKIP"
     pp CTrue = text "true"
     pp CFalse = text "false"
     pp (DotTuple x) = hcat $ punctuate (text ".") $ map pp x
     pp (Closure x) = text "{|" <> (hcat $ punctuate (comma <+> empty) $ map pp x) <> text "|}"
     pp (CallBuiltIn x [[y]]) = pp x <> lparen <> pp y <> rparen
     pp (Ifte x y z) = text "if" <+> pp x <+> text "then" $$ pp y $$ text "else" <+> pp z
     pp (AndExp x y) = pp x <+> text "and" <+> pp y
     pp (OrExp x y) = pp x <+> text "or" <+> pp y
     pp (NotExp x) = text "not" <+> pp x
     pp (Fun1 x y) = pp x <> pp y
     pp (Fun2 fun x y) =  pp x <+> pp fun <+> pp y
     pp (Lambda patt x) = text "\\" <+> (hcat $ punctuate (comma <+> empty) $ map pp patt) <+> text "@" <+> pp x
     pp (PrefixExp x l_comm_field y) = pp x <> (vcat $ map pp l_comm_field) <+> text "->" <+> pp y 
     pp (ProcSharing x_middle x_left x_right) = pp x_left <> text "[|" <> pp x_middle <> text "|]" <> pp x_right
     pp (Let decl x) = text "let" $$ (vcat $ map pp decl) $$ text "within" <+> pp x
     pp (CallFunction x [[y]]) = pp x <> lparen <> pp y <> rparen
     pp (ProcLinkParallel list x y) = pp x <+> pp list <+> pp y
--     pp (ProcRepInternalChoice list x) = pp list  <+> text "@" <+> pp x


instance PP CompGen where
     pp (Generator patt x) = pp patt <> text ":" <> pp x

comp_gen_list :: [LCompGen] -> Doc
comp_gen_list l = hcat $ punctuate empty (map pp l)

instance PP BuiltIn where
     pp (BuiltIn const) = pp const  

--instance PP CompGen where
--     pp (Generator pattern x) = pp x <> text ":" <> pp pattern
--   pp (Guard x)

instance PP Decl where
     pp (PatBind x y) = pp x <+> text "=" <+> pp y
     pp (DataType ident list_constr) = pp ident <+> text "=" <+> (hcat $ punctuate (comma <+> empty) $ map pp list_constr)
     pp (AssertRef x s y) =  text "assert" <+> pp x <+> text s <+> pp y 
     pp (AssertBool x) = text "assert" <+> pp x <+> text ":[livelock free]"
     pp (Channel list_x ty_ref) = text "channel" <+> (hcat $ punctuate (comma <+> empty) $ map pp list_x) <+> text ":" <+> pp ty_ref
     pp (FunBind ident [fun_case]) = pp ident <> text "(" <> pp fun_case

instance PP Module where 
     pp m= vcat $ map pp (moduleDecls m)

instance PP FunCase where
     pp (FunCase [[fun_args]] x) = pp fun_args <> text ")" <+> text "=" <+> pp x 

instance PP LinkList where
     pp (LinkList list_link) = lbrack <> (hcat $ punctuate (comma <+> empty) $ map pp list_link) <> rbrack

instance PP Link where
     pp (Link x y) =  pp x <> text "<->" <> pp y

instance PP Constructor where
     pp (Constructor ident ty_ref) = pp ident <+> pp ty_ref

instance (PP x) => PP (Maybe x) where
     pp (Just x) = pp x
     pp Nothing = empty

--instance PP Funcase where -- siehe emptySet
--     pp (Funcase [[arg]] x) = text "(" <>  <> text ")"  <+> text "=" <+> pp x

instance PP TypeDef where 
--     pp (TypeTuple [x]) = 
     pp (TypeDot x) =  (hcat $ punctuate (text ".") $ map pp x)

instance PP Pattern where
     pp (IntPat x) = integer x
     pp (VarPat x) = pp x
     pp EmptySetPat = text "{" <> text "}" 

instance PP CommField where
     pp (InComm x) = text "?" <> pp x
     pp (OutComm x) = text "!" <> pp x 

instance PP Const where
     pp F_true = text "true"
     pp F_false = text "false"
     pp F_not = text "not"
     pp F_and = text "and"
     pp F_or = text "or"
     pp F_STOP = text "STOP"
     pp F_SKIP = text "SKIP"
     pp F_Mult = text "*"
     pp F_Div = text ":"
     pp F_Add = text "+"
     pp F_Sub = text "-"
     pp F_Eq = text "=="
     pp F_NEq = text "!=" 
     pp F_ExtChoice = text "[]"

to_PString :: LModule -> String
to_PString mod = render (pp mod) 


runPretty :: FilePath -> IO String 
runPretty fname = 
  do 
   mod <- parseFile fname
   return (to_PString mod) --(render (pp mod))  

