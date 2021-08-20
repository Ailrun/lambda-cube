module LambdaCube.SystemFw_.Evaluator where

import           LambdaCube.SystemFw_.Ast

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b) = LCLam t b

substitute :: Int -> LCValue -> LCTerm -> LCTerm
substitute n v = go n
  where
    go m e@(LCVar l) = if m == l then liftLCValue v else e
    go m (LCLam t b) = LCLam t (substitute (m + 1) v b)
    go m (LCApp f a) = LCApp (go m f) (go m a)

evaluate :: LCTerm -> LCValue
evaluate = go
  where
    go (LCVar _) = error "Did you really type check this?"
    go (LCLam t b) = LCValLam t b
    go (LCApp f a)
      | LCValLam _ b <- go f
      , v <- go a
      = go (substitute 0 v b)
      | otherwise
      = error "Did you really type check this?"
