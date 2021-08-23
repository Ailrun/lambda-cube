module LambdaCube.SystemF.Evaluator where

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
      = go $ substituteValue 0 v b
      | otherwise
      = error "Did you really type check this?"
    go (LCTLam b) = LCValTLam b
    go (LCTApp f t)
      | LCValTLam b <- go f
      = go $ substituteType 0 t b
      | otherwise
      = error "Did you really type check this?"
