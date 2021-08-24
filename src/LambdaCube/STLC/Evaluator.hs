module LambdaCube.STLC.Evaluator
  ( evaluate
  ) where

import           LambdaCube.STLC.Ast
import           LambdaCube.STLC.Substitution

evaluate :: LCTerm -> LCValue
evaluate = go
  where
    go (LCVar _) = error "Did you really type check this?"
    go (LCLam t b) = LCValLam t b
    go (LCApp f a)
      | LCValLam _ b <- go f
      , v <- go a
      = go $ substituteValue v 0 b
