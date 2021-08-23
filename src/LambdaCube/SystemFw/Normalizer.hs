module LambdaCube.SystemFw.Normalizer where

import           LambdaCube.SystemFw.Ast
import           LambdaCube.SystemFw.Substitution

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar n) = LCNormNeut $ LCNeutVar n
    go (LCLam t b) = LCNormLam t $ go b
    go (LCTLam k b) = LCNormTLam k $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam _ b   -> substituteNormalInNormal 0 a' b
        LCNormTLam _ _  -> error "Did you really type check this?"
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
      where
        a' = go a
    go (LCTApp f t) =
      case go f of
        LCNormLam _ _   -> error "Did you really type check this?"
        LCNormTLam _ b  -> substituteTypeInNormal 0 t b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutTApp` t
