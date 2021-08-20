module LambdaCube.SystemFw.Evaluator where

import           LambdaCube.SystemFw.Ast
import           LambdaCube.SystemFw.TypeChecker (substituteType)

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b)  = LCLam t b
liftLCValue (LCValTLam k b) = LCTLam k b

substitute :: Int -> LCValue -> LCTerm -> LCTerm
substitute n v = go n
  where
    go m e@(LCVar l)  = if m == l then liftLCValue v else e
    go m (LCLam t b)  = LCLam t $ go (m + 1) b
    go m (LCApp f a)  = go m f `LCApp` go m a
    go m (LCTLam k b) = LCTLam k $ go m b
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
    go (LCTLam k b) = LCValTLam k b
    go (LCTApp f t)
      | LCValTLam _ b <- go f
      = go $ substituteType 0 t b
      | otherwise
      = error "Did you really type check this?"
