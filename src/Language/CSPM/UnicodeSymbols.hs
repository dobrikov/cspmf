----------------------------------------------------------------------------
-- |
-- Module      :  Language.CSPM.UnicodeSymbols
--
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Unicode symbols for CPSM operators

module Language.CSPM.UnicodeSymbols
where

import Language.CSPM.TokenClasses
import qualified Data.Array as Array
import qualified Data.Map as Map

unicodeSymbols :: [(Char, PrimToken, String)]
unicodeSymbols = [
      ('\172'    ,T_not               ,"not"     ) --   ¬
--    , ('\964'    ,T_tau               ,"tau"     ) --   τ
--    , ('\8714'   ,T_member            ,"member"  ) --   ∊
    , ('\8741'   ,T_parallel          ,"||"      ) --   ∥
    , ('\8743'   ,T_and               ,"and"     ) --   ∧
    , ('\8744'   ,T_or                ,"or"      ) --   ∨
--    , ('\8745'   ,T_inter             ,"inter"   ) --   ∩
--    , ('\8746'   ,T_union             ,"union"   ) --   ∪
    , ('\8800'   ,T_neq               ,"!="      ) --   ≠
    , ('\8801'   ,T_eq                ,"=="      ) --   ≡
    , ('\8804'   ,T_le                ,"<="      ) --   ≤
    , ('\8805'   ,T_ge                ,">="      ) --   ≥
    , ('\8849'   ,T_Refine            ,"[="      ) --   ⊑
    , ('\8851'   ,T_sqcap             ,"|~|"     ) --   ⊓
    , ('\8898'   ,T_Inter             ,"Inter"   ) --   ⋂
    , ('\8899'   ,T_Union             ,"Union"   ) --   ⋃
    , ('\9651'   ,T_triangle          ,"/\\"     ) --   △
    , ('\9655'   ,T_rhd               ,"[>"      ) --   ▷
    , ('\9656'   ,T_exp               ,"|>"      ) --   ▸
    , ('\9723'   ,T_box               ,"[]"      ) --   ◻
    , ('\10132'  ,T_rightarrow        ,"->"      ) --   ➔
    , ('\10214'  ,T_openBrackBrack    ,"[["      ) --   ⟦
    , ('\10215'  ,T_closeBrackBrack   ,"]]"      ) --   ⟧
    , ('\10216'  ,T_lt                ,"<"       ) --   ⟨
    , ('\10217'  ,T_gt                ,">"       ) --   ⟩
    , ('\10231'  ,T_leftrightarrow    ,"<->"     ) --   ⟷
    ]

{-
tableLine (char,tok,string) = concat
      [ "    , ("
      , tab 10 $ ("'\\" ++ (show $ fromEnum char) ++ "'"),","
      , tab 20 $ show tok, ","
      , tab 10 $ show string, ") --   "
      , [char], "\n"
      ]
  where
    tab x s = take x $ s ++ repeat ' '
-}


lookupDefaultSymbol :: PrimToken -> (Maybe (Char,String))
lookupDefaultSymbol = (Array.!) table
  where
    table :: Array.Array PrimToken (Maybe (Char,String))
    table = (Array.//)
              (Array.listArray (minBound,maxBound) $ repeat Nothing)
              [(tok,Just (uni,ascii)) | (uni,tok,ascii) <- unicodeSymbols]

lookupToken :: Char -> Maybe PrimToken
lookupToken = flip (Map.lookup) symbolMap
  where
    symbolMap = Map.fromList [(uni,tok) | (uni,tok,_string) <- unicodeSymbols]