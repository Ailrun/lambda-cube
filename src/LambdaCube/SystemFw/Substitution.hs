module LambdaCube.SystemFw.Substitution where

import           LambdaCube.SystemFw.Ast
import           LambdaCube.SystemFw.Lifter

substituteType :: Int -> LCType -> LCTerm -> LCTerm
substituteType n v = go n
  where
    go _ e@(LCVar _)  = e
    go m (LCLam t b)  = LCLam (substituteTypeInType m v t) $ go m b
    go m (LCApp f a)  = go m f `LCApp` go m a
    go m (LCTLam k b) = LCTLam k $ go (m + 1) b
    go m (LCTApp f t) = go m f `LCTApp` substituteTypeInType m v t

substituteTypeInType :: Int -> LCType -> LCType -> LCType
substituteTypeInType n v = go n
  where
    go _ LCBase        = LCBase
    go m e@(LCTVar l)  = if m == l then v else e
    go m (LCArr a b)   = go m a `LCArr` go m b
    go m (LCUniv k a)  = LCUniv k $ go (m + 1) a
    go m (LCTTLam k b) = LCTTLam k $ go (m + 1) b
    go m (LCTTApp f a) = go m f `LCTTApp` go m a

substituteValue :: Int -> LCValue -> LCTerm -> LCTerm
substituteValue n v = go n
  where
    go m e@(LCVar l)  = if m == l then liftLCValue v else e
    go m (LCLam t b)  = LCLam t $ go (m + 1) b
    go m (LCApp f a)  = go m f `LCApp` go m a
    go m (LCTLam k b) = LCTLam k $ go m b
    go m (LCTApp f t) = go m f `LCTApp` t

substituteNormalInNormal :: Int -> LCNormalTerm -> LCNormalTerm -> LCNormalTerm
substituteNormalInNormal n v = go n
  where
    go m (LCNormLam t b)  = LCNormLam t $ go (m + 1) b
    go m (LCNormTLam k b) = LCNormTLam k $ go m b
    go m (LCNormNeut nt)  = substituteNormalInNeutral m v nt

substituteNormalInNeutral :: Int -> LCNormalTerm -> LCNeutralTerm -> LCNormalTerm
substituteNormalInNeutral n v = go n
  where
    go m e@(LCNeutVar l)
      | m == l = v
      | otherwise = LCNormNeut e
    go m (LCNeutApp f a) =
      case go m f of
        LCNormLam _ b  -> substituteNormalInNormal 0 a' b
        LCNormTLam _ _ -> error "Did you really type check this?"
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = substituteNormalInNormal m v a
    go m (LCNeutTApp f t) =
      case go m f of
        LCNormLam _ _  -> error "Did you really type check this?"
        LCNormTLam _ b -> substituteTypeInNormal 0 t b
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutTApp` t

substituteTypeInNormal :: Int -> LCType -> LCNormalTerm -> LCNormalTerm
substituteTypeInNormal n v = go n
  where
    go m (LCNormLam t b)  = LCNormLam (substituteTypeInType m v t) $ go m b
    go m (LCNormTLam k b) = LCNormTLam k $ go (m + 1) b
    go m (LCNormNeut nt)  = substituteTypeInNeutral m v nt

substituteTypeInNeutral :: Int -> LCType -> LCNeutralTerm -> LCNormalTerm
substituteTypeInNeutral n v = go n
  where
    go _ e@(LCNeutVar _) = LCNormNeut e
    go m (LCNeutApp f a) =
      case go m f of
        LCNormLam _ b  -> substituteNormalInNormal 0 a' b
        LCNormTLam _ _ -> error "Did you really type check this?"
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = substituteTypeInNormal m v a
    go m (LCNeutTApp f t) =
      case go m f of
        LCNormLam _ _  -> error "Did you really type check this?"
        LCNormTLam _ b -> substituteTypeInNormal 0 t' b
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutTApp` t'
      where
        t' = substituteTypeInType m v t
