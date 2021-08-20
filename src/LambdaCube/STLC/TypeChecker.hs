module LambdaCube.STLC.TypeChecker where

import           Data.List           (uncons)
import           LambdaCube.STLC.Ast

infer :: LCTerm -> Maybe LCType
infer = go []
  where
    go l (LCVar n) = fmap fst . uncons $ drop n l
    go l (LCLam t b) = LCArr t <$> go (t : l) b
    go l (LCApp f a)
      | Just (LCArr at' rt) <- go l f
      , Just at <- go l a
      , at == at'
      = Just rt
      | otherwise
      = Nothing
