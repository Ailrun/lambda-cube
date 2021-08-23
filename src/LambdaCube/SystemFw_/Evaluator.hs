module LambdaCube.SystemFw_.Evaluator where

import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.Substitution

evaluate :: LCTerm -> LCValue
evaluate = go
  where
    go (LCVar _) = error "Did you really type check this?"
    go (LCLam t b) = LCValLam t b
    go (LCApp f a)
      | LCValLam _ b <- go f
      , v <- go a
      = go (substituteValue 0 v b)
      | otherwise
      = error "Did you really type check this?"
