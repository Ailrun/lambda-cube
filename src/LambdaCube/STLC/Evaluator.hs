module LambdaCube.STLC.Evaluator where

import           LambdaCube.STLC.Ast

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b) = LCLam t b

substitute :: Int -> LCValue -> LCTerm -> LCTerm
substitute n v e@(LCVar m)
  | n == m = liftLCValue v
  | otherwise = e
substitute n v (LCLam t b) = LCLam t $ substitute (n + 1) v b
substitute n v (LCApp f a) = substitute n v f `LCApp` substitute n v a

-- strict eval
evaluate :: LCTerm -> LCValue
evaluate = go
  where
    go (LCVar _) = error "Did you really type check this?"
    go (LCLam t b) = LCValLam t b
    go (LCApp f a)
      | LCValLam _ b <- go f
      , v <- go a
      = go (substitute 0 v b)
