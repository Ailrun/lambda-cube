module LambdaCube.SystemF.Evaluator
  ( evaluate
  ) where

import           LambdaCube.SystemF.Ast
import           LambdaCube.SystemF.Substitution

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
    go (LCTLam b) = LCValTLam b
    go (LCTApp f t)
      | LCValTLam b <- go f
      = go $ substituteType t 0 b
      | otherwise
      = error "Did you really type check this?"
