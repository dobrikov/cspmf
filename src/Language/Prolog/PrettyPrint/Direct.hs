{-
A very simple prettyprinter for Prolog-Syntax
Just some newtypes and smart-constructors

this is all buggy
todo:
 separate package for Prolog-Syntax
 todo :: remove overloading /classes from Module (for TERM and CLAUSE)
 for atom, overloading is probably ok
 make seperate nonoverloaded functions for building terms
-}
{-# LANGUAGE TypeSynonymInstances , FlexibleInstances, UndecidableInstances  #-}
{-# LANGUAGE OverlappingInstances #-}

module Language.Prolog.PrettyPrint.Direct

where

import Text.PrettyPrint
import Data.Char

renderProlog :: Doc -> String
renderProlog a = renderStyle (Style PageMode 60 1.5) a

newtype Atom = Atom {unAtom :: Doc}
newtype Term = Term {unTerm :: Doc}
newtype Predicate = Predicate {unPredicate :: Doc}
newtype Clause = Clause {unClause :: Doc}
newtype Decl = Decl {unDecl :: Doc}

newtype Quote = Quote String

class ATOM a where atom :: a -> Atom

instance ATOM Atom  where atom = id
instance ATOM String where atom = Atom . text . quoteString
instance ATOM Integer where atom = Atom . integer
instance ATOM Int where atom = Atom . int
instance ATOM Double where atom = Atom . double 
instance ATOM Quote where atom (Quote s) = Atom $ text $ quoteString s

quoteString :: String -> String
quoteString a 
  = "'"++(escapeQuote $ tail $ reverse $ tail $ reverse $ show a )++"'"


class TERM t where term :: t -> Term

instance TERM Term where term = id
instance TERM Atom where term = Term . unAtom

class TERMLIST t where termList :: t -> [Term]
instance TERM t => TERMLIST [t]
  where termList = map term
instance (TERM t1,TERM t2) => TERMLIST (t1,t2)
  where termList (a,b) = [term a,term b]
instance (TERM t1,TERM t2,TERM t3) => TERMLIST (t1,t2,t3)
  where termList (a,b,c) = [term a,term b,term c]
instance (TERM t1,TERM t2,TERM t3,TERM t4) => TERMLIST (t1,t2,t3,t4)
  where termList (a,b,c,d) = [term a,term b,term c,term d]
instance (TERM t1,TERM t2,TERM t3,TERM t4,TERM t5) => TERMLIST (t1,t2,t3,t4,t5)
  where termList (a,b,c,d,e) = [term a,term b,term c,term d,term e]
instance (TERM t1,TERM t2,TERM t3,TERM t4,TERM t5,TERM t6) => TERMLIST (t1,t2,t3,t4,t5,t6)
  where termList (a,b,c,d,e,f) = [term a,term b,term c,term d,term e,term f]
instance (TERM t1,TERM t2,TERM t3,TERM t4,TERM t5,TERM t6,TERM t7)
  => TERMLIST (t1,t2,t3,t4,t5,t6,t7)
  where termList (a,b,c,d,e,f,g) = [term a,term b,term c,term d,term e,term f,term g]

{-
This is the default-case.
It overlapps all the other cases
-}
instance TERM t => TERMLIST t where termList a = [term a]

nTerm :: (ATOM f, TERMLIST ch) => f -> ch -> Term
nTerm f ch = Term $
  (unAtom $ atom f) <> if null childs
      then empty
      else parens $ hcat $ punctuate comma 
                $ map ( unTerm . term ) childs
  where childs = termList ch

aTerm :: ATOM f => f -> Term
aTerm = Term . unAtom . atom

pList :: TERM ch => [ch] -> Term
pList l
  = Term $ brackets $ hcat 
      $ punctuate comma $ map (unTerm . term )l

plVar  :: String -> Term
plVar [] = error "plVar : empty Sting"
plVar a@(h:_)
  = if isUpper h || h=='_'
      then Term $ text $ escapeBadChars a
      else error ("lowercase var:" ++ a)
        where
           escapeBadChars x = concatMap esc x
           esc '\'' = "_quote"
           esc x = [x]

plWildCard :: Term
plWildCard = Term $ text "_"

class PREDICATE p where predicate :: p -> Predicate
instance PREDICATE Predicate where predicate = id
instance TERM t => PREDICATE t where predicate = Predicate . unTerm . term

class CLAUSE c where clause :: c -> Clause
instance CLAUSE Clause where clause = id
instance PREDICATE p => CLAUSE p
  where clause x = Clause ( (unPredicate $ predicate x) <> text ".")
 
nClause :: (PREDICATE h, PREDICATE b) => h -> [b] -> Clause
nClause h b
  = Clause $ (unPredicate $ predicate h) <+> text ":-"
      $$ nest 3 (vcat $ punctuate comma $ map (unPredicate . predicate) b)
      <> text "."

singleClause :: Clause -> Decl
singleClause (Clause x) = Decl x

declGroup :: [Clause] -> Decl
declGroup l = Decl $ vcat $ map unClause l

plPrg :: [Decl] -> Doc
plPrg l = vcat $ map unDecl l

escapeQuote :: String -> String
escapeQuote x = concatMap escapeQ x
  where escapeQ '\'' = "\\'"
        escapeQ c = [c]
