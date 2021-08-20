module LambdaCube.STLC.Normalizer where

import           LambdaCube.STLC.Ast

substituteNormal :: Int -> LCNormalTerm -> LCNormalTerm -> LCNormalTerm
substituteNormal n nv = go n
  where
    go m (LCNormLam t b)   = LCNormLam t $ go (m + 1) b
    go m (LCNormNeut neut) = substituteNeutral m nv neut

substituteNeutral :: Int -> LCNormalTerm -> LCNeutralTerm -> LCNormalTerm
substituteNeutral n nv = go n
  where
    go m e@(LCNeutVar l) = if m == l then nv else LCNormNeut e
    go m (LCNeutApp f a) =
      case go m f of
        LCNormLam _ b   -> substituteNormal 0 a' b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
      where
        a' = substituteNormal m nv a

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar n) = LCNormNeut $ LCNeutVar n
    go (LCLam t b) = LCNormLam t $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam t b   -> LCNormLam t $ substituteNormal 0 a' b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
      where
        a' = go a
