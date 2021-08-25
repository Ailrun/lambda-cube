module LambdaCube.SystemFw.Normalizer
  ( normalize
  ) where

import           LambdaCube.SystemFw.Ast
import           LambdaCube.SystemFw.Substitution

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar x) = LCNormNeut $ LCNeutVar x
    go (LCLam t b) = LCNormLam t $ go b
    go (LCTLam k b) = LCNormTLam k $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam _ b  -> substituteNormalInNormal a' 0 b
        LCNormTLam _ _ -> error "Did you really type check this?"
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = go a
    go (LCTApp f t) =
      case go f of
        LCNormLam _ _  -> error "Did you really type check this?"
        LCNormTLam _ b -> substituteTypeInNormal t 0 b
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutTApp` t
