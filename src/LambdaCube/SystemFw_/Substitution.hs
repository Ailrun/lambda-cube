module LambdaCube.SystemFw_.Substitution where

import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.Lifter

substituteTypeInType :: Int -> LCType -> LCType -> LCType
substituteTypeInType n v = go n
  where
    go _ LCBase        = LCBase
    go m e@(LCTVar l)  = if m == l then v else e
    go m (LCArr a b)   = go m a `LCArr` go m b
    go m (LCTTLam k b) = LCTTLam k $ go (m + 1) b
    go m (LCTTApp f a) = go m f `LCTTApp` go m a

substituteValue :: Int -> LCValue -> LCTerm -> LCTerm
substituteValue n v = go n
  where
    go m e@(LCVar l) = if m == l then liftLCValue v else e
    go m (LCLam t b) = LCLam t (go (m + 1) b)
    go m (LCApp f a) = LCApp (go m f) (go m a)

substituteNormalInNormal :: Int -> LCNormalTerm -> LCNormalTerm -> LCNormalTerm
substituteNormalInNormal n v = go n
  where
    go m (LCNormLam t b) = LCNormLam t $ go (m + 1) b
    go m (LCNormNeut nt) = substituteNormalInNeutral m v nt

substituteNormalInNeutral :: Int -> LCNormalTerm -> LCNeutralTerm -> LCNormalTerm
substituteNormalInNeutral n v = go n
  where
    go m e@(LCNeutVar l) = if m == l then v else LCNormNeut e
    go m (LCNeutApp f a) =
      case go m f of
        LCNormLam _ b -> substituteNormalInNormal 0 a' b
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = substituteNormalInNormal m v a
