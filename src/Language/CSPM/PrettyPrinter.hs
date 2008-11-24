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

module Language.CSPM.PrettyPrinter
where

import Text.PrettyPrint as PrettyPrint hiding (char)
import qualified Text.PrettyPrint as PrettyPrint
import Language.CSPM.AST

import Data.Maybe
import Data.Typeable (Typeable)
import Data.Generics.Basics (Data)
import Data.Generics.Instances ()
import Data.Ix
import Data.IntMap (IntMap)

class PP x where pp :: x-> Doc

instance (PP x) => PP (Labeled x) where 
   pp = pp . unLabel

instance PP Ident where pp = text . unIdent

instance PP BuiltIn where 
  pp (BuiltIn fun) =  text $ show fun   

instance PP Exp where 
     pp (Var x) = pp x
     pp (IntExp x) = integer x
     pp (SetEnum [x]) = set_enum pp [x]
     pp (ListEnum [x]) = list_enum pp [x]
--     pp (SetOpen x) 
--     pp (ListOpen x)
--     pp SetComprehension ([LExp],[LCompGen]) =
     pp (SetClose (x,y)) = lbrace <> pp x <> text ".." <> pp y <> rbrace
     pp (ListClose (x,y)) = lbrack <> pp x <> text ".." <> pp y <> rbrack
     pp BoolSet = text "Bool"
     pp IntSet = text "Int"
     pp Events = text "Events"
     pp Stop = text "STOP"
     pp Skip = text "SKIP"
     pp CTrue = text "true"
     pp CFalse = text "false"
     pp (DotTuple [x]) = dot_tuple pp [x]
     pp (Closure [x]) = closure pp [x]
     pp (CallBuiltIn x [[y]]) = pp x <> lparen <> pp y <> rparen
     pp (Ifte x y z) = text "if" <+> pp x <+> text "then" $$ pp y $$ text "else" <+> pp z
     pp (AndExp x y) = pp x <+> text "and" <+> pp y
     pp (OrExp x y) = pp x <+> text "or" <+> pp y
     pp (NotExp x) = text "not" <+> pp x
     pp (Fun1 x y) = pp x <> pp y
     pp (Fun2 fun x y) =  pp x <+> pp fun <+> pp y
     pp (Lambda [patt] x) = text "\\" <+> list_pattern pp [patt] <+> text "@" <+> pp x
     pp (PrefixExp x [l_comm_field] y) = pp x <> comm_field pp [l_comm_field] <+> text "->" <+> pp y 
     pp (ProcSharing x_middle x_left x_right) = pp x_left <> text "[|" <> pp x_middle <> text "|]" <> pp x_right
     pp (Let [decl] x) = text "let" $$ l_decl pp [decl] $$ text "within" <+> pp x
     pp (CallFunction x [[y]]) = pp x <> lparen <> pp y <> rparen
     pp (ProcLinkParallel list x y) = pp x <+> pp list <+> pp y
--     pp (ProcRepInternalChoice [comp_gen] x) = pp comp_gen_list <+> text "@" <+> pp x



instance PP CompGen where
     pp (Generator pattern x) = pp x <> text ":" <> pp pattern
--   pp (Guard x)

instance PP Decl where
     pp (PatBind x y) = pp x <+> text "=" <+> pp y
     pp (DataType ident [constr]) = pp ident <+> text "=" <+> l_constr pp [constr]
     pp (AssertRef x s y) =  text "assert" <+> pp x <+> text s <+> pp y 
     pp (AssertBool x) = text "assert" <+> pp x <+> text ":[livelock free]"
     pp (Channel [x] ty_ref) = text "channel" <+> list_pattern pp [x] <+> text ":" <+> pp ty_ref

instance PP LinkList where
     pp (LinkList [link]) = lbrack <> list_pattern pp [link] <> rbrack

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
     pp (TypeDot [x]) = ty_dot pp [x]

instance PP Pattern where
     pp (IntPat x) = integer x
     pp (VarPat x) = pp x
     pp EmptySetPat = text "{" <> text "}" 

instance PP CommField where
     pp (InComm x) = text "?" <> pp x
     pp (OutComm x) = text "!" <> pp x 

--instance PP Const where
--     pp ()

dot_tuple :: (x -> Doc) -> [x] -> Doc
dot_tuple _ [] = error "Empty Dot Expression"
dot_tuple f l = 
   let 
     first = f $ head l
     list = foldl (\h r -> h <> ((text ".") <+> f r)) first (tail l)
   in list

l_constr :: (x -> Doc) -> [x] -> Doc
l_constr f [] = empty
l_constr f l = 
   let 
     first = f $ head l
     list = foldl (\h r -> h <> ((text "|") <+> f r)) first (tail l)
   in list

ty_dot :: (x -> Doc) -> [x] -> Doc
ty_dot f [] = empty
ty_dot f l = 
   let 
     first = f $ head l
     list = foldl (\h r -> h <> ((text ".") <> f r)) first (tail l)
   in list

l_decl :: (x -> Doc) -> [x] -> Doc
l_decl f [] = empty
l_decl f l = 
   let 
     first = f $ head l
     list = foldl (\h r -> h $$ (f r)) first (tail l)
   in list

comm_field :: (x -> Doc) -> [x] -> Doc
comm_field f [] = empty
comm_field f l =  
   let 
     first = f $ head l
     list = foldl (\h r -> h <> (f r)) first (tail l)
   in list

list_pattern :: (x -> Doc) -> [x] -> Doc
list_pattern f l = 
   let 
     first = f $ head l
     list = foldl (\h r -> h <> (comma <+> f r)) first (tail l)
   in list

set_enum :: (x -> Doc) -> [x] -> Doc
set_enum _ [] = error "empty block"
set_enum f l = 
   let 
     first = lbrace <+> (f $ head l)
     list = foldl (\h r -> h <> (comma <+> f r)) first (tail l)
   in list <+> rbrace

list_enum :: (x -> Doc) -> [x] -> Doc
list_enum f l = 
   let 
     first = lbrack <+> (f $ head l)
     list = foldl (\h r -> h <> (comma <+> f r)) first (tail l)
   in list <+> rbrack

closure :: (x -> Doc) -> [x] -> Doc
closure f l = 
   let 
     first = text "{|" <+> (f $ head l)
     list = foldl (\h r -> h <> (comma <+> f r)) first (tail l)
   in list <+> text "|}"

