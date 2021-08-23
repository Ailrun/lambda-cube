module LambdaCube.STLC.Normalizer where

import           LambdaCube.STLC.Ast
import           LambdaCube.STLC.Substitution

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar n) = LCNormNeut $ LCNeutVar n
    go (LCLam t b) = LCNormLam t $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam t b   -> LCNormLam t $ substituteNormalInNormal 0 a' b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
      where
        a' = go a
