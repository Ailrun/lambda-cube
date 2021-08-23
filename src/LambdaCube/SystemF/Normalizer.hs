module LambdaCube.SystemF.Normalizer where

import           LambdaCube.SystemF.Ast
import           LambdaCube.SystemF.Substitution

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar n) = LCNormNeut $ LCNeutVar n
    go (LCLam t b) = LCNormLam t $ go b
    go (LCTLam b) = LCNormTLam $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam _ b   -> substituteNormalInNormal 0 a' b
        LCNormTLam _    -> error "Did you really type check this?"
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
      where
        a' = go a
    go (LCTApp f t) =
      case go f of
        LCNormLam _ _   -> error "Did you really type check this?"
        LCNormTLam b    -> substituteTypeInNormal 0 t b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutTApp` t
