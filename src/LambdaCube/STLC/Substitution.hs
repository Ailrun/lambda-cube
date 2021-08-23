module LambdaCube.STLC.Substitution where

import           LambdaCube.STLC.Ast
import           LambdaCube.STLC.Lifter

substituteValue :: Int -> LCValue -> LCTerm -> LCTerm
substituteValue n v = go n
  where
    go m e@(LCVar l) = if m == l then liftLCValue v else e
    go m (LCLam t b) = LCLam t $ go (m + 1) b
    go m (LCApp f a) = go m f `LCApp` go m a

substituteNormalInNormal :: Int -> LCNormalTerm -> LCNormalTerm -> LCNormalTerm
substituteNormalInNormal n nv = go n
  where
    go m (LCNormLam t b)   = LCNormLam t $ go (m + 1) b
    go m (LCNormNeut neut) = substituteNormalInNeutral m nv neut

substituteNormalInNeutral :: Int -> LCNormalTerm -> LCNeutralTerm -> LCNormalTerm
substituteNormalInNeutral n nv = go n
  where
    go m e@(LCNeutVar l) = if m == l then nv else LCNormNeut e
    go m (LCNeutApp f a) =
      case go m f of
        LCNormLam _ b   -> substituteNormalInNormal 0 a' b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
      where
        a' = substituteNormalInNormal m nv a
