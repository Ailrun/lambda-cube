module LambdaCube.STLC.Normalizer where

import           LambdaCube.STLC.Ast

substituteNormal :: Int -> LCNormalTerm -> LCNormalTerm -> LCNormalTerm
substituteNormal n nv (LCNormLam t b) = LCNormLam t $ substituteNormal (n + 1) nv b
substituteNormal n nv (LCNormNeut neut) = substituteNeutral n nv neut

substituteNeutral :: Int -> LCNormalTerm -> LCNeutralTerm -> LCNormalTerm
substituteNeutral n nv e@(LCNeutVar m)
  | n == m = nv
  | otherwise = LCNormNeut e
substituteNeutral n nv (LCNeutApp f a) =
  case f' of
    LCNormLam _ b   -> substituteNormal 0 a' b
    LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
  where
    f' = substituteNeutral n nv f
    a' = substituteNormal n nv a

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar n) = LCNormNeut $ LCNeutVar n
    go (LCLam t b) = LCNormLam t $ go b
    go (LCApp f a) =
      case nf of
        LCNormLam t b   -> LCNormLam t $ substituteNormal 0 na b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` na
      where
        nf = go f
        na = go a
