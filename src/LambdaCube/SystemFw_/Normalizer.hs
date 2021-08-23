module LambdaCube.SystemFw_.Normalizer where

import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.Substitution
import           LambdaCube.SystemFw_.TypeChecker  (reduceType)

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar n) = LCNormNeut $ LCNeutVar n
    go (LCLam t b) = LCNormLam (reduceType t) $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam _ b   -> substituteNormalInNormal 0 a' b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
      where
        a' = go a
