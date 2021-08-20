module LambdaCube.SystemF.Evaluator where

import           LambdaCube.SystemF.Ast
import           LambdaCube.SystemF.TypeChecker (substituteType)

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b) = LCLam t b
liftLCValue (LCValTLam b)  = LCTLam b

substitute :: Int -> LCValue -> LCTerm -> LCTerm
substitute n v e@(LCVar m)
  | n == m = liftLCValue v
  | otherwise = e
substitute n v (LCLam t b) = LCLam t (substitute (n + 1) v b)
substitute n v (LCApp f a) = LCApp (substitute n v f) (substitute n v a)
substitute n v (LCTLam b) = LCTLam (substitute n v b)
substitute n v (LCTApp f t) = LCTApp (substitute n v f) t

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
      | otherwise
      = error "Did you really type check this?"
    go (LCTLam b) = LCValTLam b
    go (LCTApp f t)
      | LCValTLam b <- go f
      = go (substituteType 0 t b)
      | otherwise
      = error "Did you really type check this?"
