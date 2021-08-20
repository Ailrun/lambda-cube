module LambdaCube.STLC.Evaluator where

import           LambdaCube.STLC.Ast

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b) = LCLam t b

substituteValue :: Int -> LCValue -> LCTerm -> LCTerm
substituteValue n v = go n
  where
    go m e@(LCVar l) = if m == l then liftLCValue v else e
    go m (LCLam t b) = LCLam t $ go (m + 1) b
    go m (LCApp f a) = go m f `LCApp` go m a

evaluate :: LCTerm -> LCValue
evaluate = go
  where
    go (LCVar _) = error "Did you really type check this?"
    go (LCLam t b) = LCValLam t b
    go (LCApp f a)
      | LCValLam _ b <- go f
      , v <- go a
      = go $ substituteValue 0 v b
