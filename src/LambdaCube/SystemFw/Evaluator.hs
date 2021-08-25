module LambdaCube.SystemFw.Evaluator
  ( evaluate
  ) where

import           LambdaCube.SystemFw.Ast
import           LambdaCube.SystemFw.Substitution

evaluate :: LCTerm -> LCValue
evaluate = go
  where
    go (LCVar _) = error "Did you really type check this?"
    go (LCLam t b) = LCValLam t b
    go (LCApp f a)
      | LCValLam _ b <- go f
      , v <- go a
      = go $ substituteValue v 0 b
      | otherwise
      = error "Did you really type check this?"
    go (LCTLam k b) = LCValTLam k b
    go (LCTApp f t)
      | LCValTLam _ b <- go f
      = go $ substituteType t 0 b
      | otherwise
      = error "Did you really type check this?"
