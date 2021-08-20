module LambdaCube.SystemF.Evaluator where

import           LambdaCube.SystemF.Ast
import           LambdaCube.SystemF.TypeChecker (substituteType)

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b) = LCLam t b
liftLCValue (LCValTLam b)  = LCTLam b

substitute :: Int -> LCValue -> LCTerm -> LCTerm
substitute n v = go n
  where
    go m e@(LCVar l)  = if m == l then liftLCValue v else e
    go m (LCLam t b)  = LCLam t $ go (m + 1) b
    go m (LCApp f a)  = go m f `LCApp` go m a
    go m (LCTLam b)   = LCTLam $ go m b
    go m (LCTApp f t) = go m f `LCTApp` t

evaluate :: LCTerm -> LCValue
evaluate = go
  where
    go (LCVar _) = error "Did you really type check this?"
    go (LCLam t b) = LCValLam t b
    go (LCApp f a)
      | LCValLam _ b <- go f
      , v <- go a
      = go $ substitute 0 v b
      | otherwise
      = error "Did you really type check this?"
    go (LCTLam b) = LCValTLam b
    go (LCTApp f t)
      | LCValTLam b <- go f
      = go $ substituteType 0 t b
      | otherwise
      = error "Did you really type check this?"
