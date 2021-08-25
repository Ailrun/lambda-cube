module LambdaCube.STLC.Normalizer
  ( normalize
  ) where

import           LambdaCube.STLC.Ast
import           LambdaCube.STLC.Substitution

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar x) = LCNormNeut $ LCNeutVar x
    go (LCLam t b) = LCNormLam t $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam _ b -> substituteNormalInNormal a' 0 b
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = go a
