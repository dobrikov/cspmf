-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.AstToProlog
-- Copyright   :  (c) Fontaine 2012
-- License     :  BSD3
--
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Convert an AST to Prolog. An experiment with the new GHC-Generic extentions
-- This would be more compact with SYB.

{-# LANGUAGE TypeOperators,FlexibleInstances, FlexibleContexts, DefaultSignatures, OverlappingInstances #-}

module Language.CSPM.AstToProlog
  (
    toProlog
  )
where

import Language.CSPM.Rename (ModuleFromRenaming)
import Language.CSPM.AST as AST
import Language.CSPM.CompileAstToProlog (mkSrcLoc)
import qualified Language.Prolog.PrettyPrint.Direct as Prolog (unTerm,atom,unAtom)
import Language.CSPM.SrcLoc as SrcLoc

import GHC.Generics as Generics
import Text.PrettyPrint
import Data.Array.IArray as Array
import qualified Data.IntMap as IntMap

toProlog :: TP d => d -> Doc
toProlog = tp

class GTP f where
    gtp :: f a -> Doc

class GTPL f where
    gtpl :: Doc -> f a -> Doc

class TP f where
    tp :: f -> Doc
    default tp :: (Generic f, GTP (Rep f)) => f -> Doc
    tp = gtp . from

class TPL f where
    tpl :: Doc -> f -> Doc
    default tpl :: (Generic f, GTPL (Rep f)) => Doc -> f -> Doc
    tpl l = gtpl l . from


instance TPL f => TP (Labeled f) where
    tp a = tpl (tp $ srcLoc a) $ unLabel a

instance TPL Decl
instance TPL Ident where tpl l = tpl l . unUIdent
instance TPL Pattern
instance TPL Exp

instance TPL UniqueIdent where
    tpl l ident = text "'UniqueIdent'" <> parens (l<> comma <> i)
      where
         i = case idType ident of
                 TransparentID -> atom $ realName ident
                 _ -> atom $ newName ident

instance TPL AssertDecl
instance TPL AST.Constructor
instance TPL TypeDef
instance TPL NATuples
instance TPL Range
instance TPL CompGen
instance TPL LinkList
instance TPL Rename
instance TPL Link
instance TPL BuiltIn
instance TPL CommField
instance TPL [Labeled CompGen] where tpl _ = tp

instance TPL RefineOp
instance TPL TauRefineOp
instance TPL FDRModels
instance TPL FdrExt
instance TPL FormulaType

instance TP ModuleFromRenaming where
    tp m = text "module" <> parens ( hcat $ punctuate comma [
         tp $ moduleSrcLoc m
        ,tp $ moduleDecls m
        ,tp $ moduleComments m
        ,tp $ modulePragmas m
        ])

instance TP Comment
instance TP SrcLoc where tp = Prolog.unTerm . mkSrcLoc

instance TP [Char] where tp = atom

instance TP f => TP [f] where
    tp l = brackets $ hcat $ punctuate comma $ map tp l

instance TP f => TP (Maybe f) where
    tp Nothing = text "none"
    tp (Just x) = tp x

instance (TP a, TP b) => TP (a,b) where
    tp (a,b) = parens (tp a <> comma <> tp b)


instance TP Integer  where  tp = integer
instance TP Int      where tp = integer . fromIntegral
instance TP e => TP (Array Int e)     where tp = tp . Array.elems
instance TP e => TP (IntMap.IntMap e) where tp = tp . IntMap.elems


instance TP Bool
instance TP Const
instance TP FunCase
instance TP AST.Selector
instance TP UniqueIdent where tp = tpl (text "none")

instance (GTP a, GTP b) => GTP (a :*: b) where
    gtp (a :*: b) = gtp a <+> comma <+> gtp b

instance (GTPL a, GTPL b) => GTPL (a :*: b) where
    gtpl l (a :*: b) = gtpl l a <+> comma <+> gtpl l b

instance (GTP l, GTP r) => GTP (l :+: r) where
    gtp (L1 l) = gtp l
    gtp (R1 r) = gtp r

instance (GTPL l, GTPL r) => GTPL (l :+: r) where
    gtpl s (L1 l) = gtpl s l
    gtpl s (R1 r) = gtpl s r

instance (GTP t, Datatype r) => GTP (M1 D r t)  where
    gtp = gtp . unM1

instance (GTPL t, Datatype r) => GTPL (M1 D r t)  where
    gtpl l = gtpl l . unM1

instance (GTP t, Generics.Constructor c) => GTP (M1 C c t)  where
    gtp x = hcat [ atom $ conName x, lparen, gtp $ unM1 x, rparen]

instance (GTPL t, Generics.Constructor c) => GTPL (M1 C c t)  where
    gtpl l x = hcat [ atom $ conName x, lparen, l, comma, gtpl l $ unM1 x, rparen]

instance (GTP t, Generics.Selector c) => GTP (M1 S c t)  where
    gtp = gtp . unM1

instance (GTPL t, Generics.Selector c) => GTPL (M1 S c t)  where
    gtpl l = gtpl l . unM1

instance TP t => GTP (K1 R t)  where
    gtp x = tp $ unK1 x

instance TP t => GTPL (K1 R t)  where
    gtpl _l x = tp $ unK1 x

-- P is deprecated and not used anymore in recent versions of base
--instance TP t => GTP (K1 P t)  where
--    gtp x = hsep [ text "rec" , lparen, tp $ unK1 x, rparen]

--instance TP t => GTPL (K1 P t)  where
--    gtpl _ x = hsep [ text "rec" , lparen, tp $ unK1 x, rparen]

instance GTP V1 where gtp _ = text "V1"
instance GTPL V1 where gtpl _ _ = text "V1"

instance GTP U1 where gtp _ = text "U1"
instance GTPL U1 where gtpl _ _ = text "U1"

atom :: String -> Doc
atom = Prolog.unAtom . Prolog.atom