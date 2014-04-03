-----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.AstToXml
-- Copyright   :  (c) Fontaine 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Fontaine@cs.uni-duesseldorf.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Convert an AST to XML

module Language.CSPM.AstToXML
  (
    moduleToXML 
   ,astToXML
   ,showTopElement
  )
where

import Text.XML.Light
import Data.Data
import Data.Generics.Aliases (extQ, ext1Q)
import Language.CSPM.AST
import Language.CSPM.SrcLoc as SrcLoc

-- | Translate a Module to XML
moduleToXML :: Module a -> Element
moduleToXML m
  = unode "Module"
    [
       unode "moduleDecls" $ astToXML $ moduleDecls m
      ,unode "modulePragmas" $ map
         (unode "Pragma" . Attr (unqual "val"))
         (modulePragmas m)
      ,unode "moduleComments" $ astToXML $ moduleComments m
    ]

-- | Translate an AST node to an XML Element.
-- This is an 'almost' totally generic translation which
-- works for any Haskell type, but it handles some special cases.
astToXML :: Data a => a -> Element
astToXML
  = genericCase `extQ` identToXML `ext1Q` labelToXML
     `ext1Q` listToXML `extQ` intToXML `extQ` commentToXML
  where
    genericCase :: Data a => a -> Element
    genericCase n = unode (showConstr $ toConstr n) $ gmapQ astToXML n

    identToXML :: Ident -> Element
    identToXML x = case x of
      Ident s -> unode "Ident" (Attr (unqual "unIdent") s)
      UIdent u -> unode "UIdent" $ uniqueIdentToXML u

    labelToXML :: Data a => Labeled a -> Element
    labelToXML l = add_attrs
        ( idAttr : location)
        ( astToXML $ unLabel l)
      where 
        idAttr = strAttr "nodeId" $ show $ unNodeId $ nodeId l
        location = srcLocAttr $ srcLoc l

    listToXML :: Data a => [a] -> Element
    listToXML = unode "list" . map astToXML

    intToXML :: Integer -> Element
    intToXML i = unode "Integer" $ strAttr "val" $ show i

    uniqueIdentToXML n = unode "UniqueIdent"
     [
      strAttr "uniqueIdentId" $ show $ uniqueIdentId n
     ,strAttr "bindingSide" $ show $ bindingSide n
     ,strAttr "bindingLoc" $ "todo: bindingLoc"
     ,strAttr "idType" $ show $ idType n
     ,strAttr "realName" $ realName n
     ,strAttr "newName" $ newName n
     ,strAttr "prologMode" $ show $ prologMode n
     ,strAttr "bindType" $ show $ bindType n
     ]

    strAttr a s = Attr (unqual a) s

    srcLocAttr :: SrcLoc.SrcLoc -> [Attr]
    srcLocAttr loc = case loc of
      SrcLoc.TokPos {} -> [
          locAttr "sLine" $ SrcLoc.getStartLine loc
        , locAttr "sCol" $ SrcLoc.getStartCol loc
        , locAttr "sPos" $ SrcLoc.getStartOffset loc
        , locAttr "len" $ SrcLoc.getTokenLen loc
        ]
      SrcLoc.TokSpan {} -> [
          locAttr "sLine" $ SrcLoc.getStartLine loc
        , locAttr "sCol" $ SrcLoc.getStartCol loc
        , locAttr "eLine" $ SrcLoc.getEndLine loc
        , locAttr "eCol" $ SrcLoc.getEndCol loc
        , locAttr "sPos" $ SrcLoc.getStartOffset loc
        , locAttr "len" $ SrcLoc.getTokenLen loc
        ]
      SrcLoc.FixedLoc {} -> [
          locAttr "sLine" $ SrcLoc.getStartLine loc
        , locAttr "sCol" $ SrcLoc.getStartCol loc
        , locAttr "eLine" $ SrcLoc.getEndLine loc
        , locAttr "eCol" $ SrcLoc.getEndCol loc
        , locAttr "sPos" $ SrcLoc.getStartOffset loc
        , locAttr "len" $ SrcLoc.getTokenLen loc
        ]
      _ -> []

    locAttr s i = Attr (unqual s) $ show i

    commentToXML :: (Comment,SrcLoc.SrcLoc) -> Element
    commentToXML (comment,loc)
       = add_attrs (srcLocAttr loc) $ case comment of
      LineComment c -> unode "LineComment" $ strAttr "val" c
      BlockComment c -> unode "BlockComment" $ strAttr "val" c
      PragmaComment c -> unode "PragmaComment" $ strAttr "val" c
