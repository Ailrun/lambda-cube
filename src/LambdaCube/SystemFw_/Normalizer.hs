module LambdaCube.SystemFw_.Normalizer where

import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.TypeChecker (reduceType)

substituteNormal :: Int -> LCNormalTerm -> LCNormalTerm -> LCNormalTerm
substituteNormal n v = go n
  where
    go m (LCNormLam t b) = LCNormLam t $ go (m + 1) b
    go m (LCNormNeut nt) = substituteNeutral m v nt

substituteNeutral :: Int -> LCNormalTerm -> LCNeutralTerm -> LCNormalTerm
substituteNeutral n v = go n
  where
    go m e@(LCNeutVar l) = if m == l then v else LCNormNeut e
    go m (LCNeutApp f a) =
      case go m f of
        LCNormLam _ b -> substituteNormal 0 a' b
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = substituteNormal m v a

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar n) = LCNormNeut $ LCNeutVar n
    go (LCLam t b) = LCNormLam (reduceType t) $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam _ b   -> substituteNormal 0 a' b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
      where
        a' = go a
