module LambdaCube.SystemF.TypeChecker where

import           Data.List                       (uncons)
import           LambdaCube.SystemF.Ast
import           LambdaCube.SystemF.Substitution

infer :: LCTerm -> Maybe LCType
infer = go []
  where
    go tl (LCVar n) = fmap fst . uncons $ drop n tl
    go tl (LCLam t b) = LCArr t <$> go (t : tl) b
    go tl (LCApp f a)
      | Just (LCArr at' rt) <- go tl f
      , Just at <- go tl a
      , at == at'
      = Just rt
      | otherwise
      = Nothing
    go tl (LCTLam b) = LCUniv <$> go tl b
    go tl (LCTApp f t)
      | Just (LCUniv rt) <- go tl f
      = Just $ substituteTypeInType 0 t rt
      | otherwise
      = Nothing
