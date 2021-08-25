module LambdaCube.SystemFw_.Normalizer
  ( normalize
  ) where

import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.Substitution
import           LambdaCube.SystemFw_.TypeChecker  (reduceType)

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar x) = LCNormNeut $ LCNeutVar x
    go (LCLam t b) = LCNormLam (reduceType t) $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam _ b -> substituteNormalInNormal a' 0 b
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = go a
