module LambdaCube.SystemFw.Substitution
  ( substituteType
  , substituteTypeInType
  , substituteValue
  , substituteNormalInNormal
  , substituteTypeInNormal
  ) where

import           LambdaCube.SystemFw.Ast
import           LambdaCube.SystemFw.Lifter

substituteType :: LCType -> Int -> LCTerm -> LCTerm
substituteType v = substDefType (v, 0)

substituteTypeInType :: LCType -> Int -> LCType -> LCType
substituteTypeInType v = substDefTypeInType (v, 0)

substituteValue :: LCValue -> Int -> LCTerm -> LCTerm
substituteValue v = substDefValue (v, 0, 0)

substituteNormalInNormal :: LCNormalTerm -> Int -> LCNormalTerm -> LCNormalTerm
substituteNormalInNormal v = substDefNormalInNormal (v, 0, 0)

substituteTypeInNormal :: LCType -> Int -> LCNormalTerm -> LCNormalTerm
substituteTypeInNormal v = substDefTypeInNormal (v, 0)

substDefType :: (LCType, Int) -> Int -> LCTerm -> LCTerm
substDefType = go
  where
    go _      _ e@(LCVar _)  = e
    go dv     p (LCLam t b)  = LCLam (substDefTypeInType dv p t) $ go dv p b
    go dv     p (LCApp f a)  = go dv p f `LCApp` go dv p a
    go (v, r) p (LCTLam k b) = LCTLam k $ go (v, r + 1) (p + 1) b
    go dv     p (LCTApp f t) = go dv p f `LCTApp` substDefTypeInType dv p t

substDefTypeInType :: (LCType, Int) -> Int -> LCType -> LCType
substDefTypeInType = go
  where
    go _      _ LCBase       = LCBase
    go dv     p e@(LCTVar q) = if p == q then shiftType dv else e
    go dv     p (LCArr a b)  = go dv p a `LCArr` go dv p b
    go (v, r) p (LCUniv k a) = LCUniv k $ go (v, r + 1) (p + 1) a
    go (v, r) p (LCTTLam k b) = LCTTLam k $ go (v, r + 1) (p + 1) b
    go dv     p (LCTTApp f a) = go dv p f `LCTTApp` go dv p a

substDefValue :: (LCValue, Int, Int) -> Int -> LCTerm -> LCTerm
substDefValue = go
  where
    go dv        x e@(LCVar y)  = if x == y then shiftValue dv else e
    go (v, r, s) x (LCLam t b)  = LCLam t $ go (v, r, s + 1) (x + 1) b
    go dv        x (LCApp f a)  = go dv x f `LCApp` go dv x a
    go (v, r, s) x (LCTLam k b) = LCTLam k $ go (v, r + 1, s) x b
    go dv        x (LCTApp f t) = go dv x f `LCTApp` t

substDefNormalInNormal :: (LCNormalTerm, Int, Int) -> Int -> LCNormalTerm -> LCNormalTerm
substDefNormalInNormal = go
  where
    go (v, r, s) x (LCNormLam t b)  = LCNormLam t $ go (v, r, s + 1) (x + 1) b
    go (v, r, s) x (LCNormTLam k b) = LCNormTLam k $ go (v, r + 1, s) x b
    go dv        x (LCNormNeut nt)  = substDefNormalInNeutral dv x nt

substDefNormalInNeutral :: (LCNormalTerm, Int, Int) -> Int -> LCNeutralTerm -> LCNormalTerm
substDefNormalInNeutral dv = go
  where
    go x e@(LCNeutVar y) = if x == y then shiftNormal dv else LCNormNeut e
    go x (LCNeutApp f a) =
      case go x f of
        LCNormLam _ b  -> substituteNormalInNormal a' 0 b
        LCNormTLam _ _ -> error "Did you really type check this?"
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = substDefNormalInNormal dv x a
    go x (LCNeutTApp f t) =
      case go x f of
        LCNormLam _ _  -> error "Did you really type check this?"
        LCNormTLam _ b -> substituteTypeInNormal t 0 b
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutTApp` t

substDefTypeInNormal :: (LCType, Int) -> Int -> LCNormalTerm -> LCNormalTerm
substDefTypeInNormal = go
  where
    go dv     p (LCNormLam t b)  = LCNormLam (substDefTypeInType dv p t) $ go dv p b
    go (v, r) p (LCNormTLam k b) = LCNormTLam k $ go (v, r + 1) (p + 1) b
    go dv     p (LCNormNeut nt)  = substDefTypeInNeutral dv p nt

substDefTypeInNeutral :: (LCType, Int) -> Int -> LCNeutralTerm -> LCNormalTerm
substDefTypeInNeutral = go
  where
    go _  _ e@(LCNeutVar _) = LCNormNeut e
    go dv p (LCNeutApp f a) =
      case go dv p f of
        LCNormLam _ b  -> substituteNormalInNormal a' 0 b
        LCNormTLam _ _ -> error "Did you really type check this?"
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = substDefTypeInNormal dv p a
    go dv p (LCNeutTApp f t) =
      case go dv p f of
        LCNormLam _ _  -> error "Did you really type check this?"
        LCNormTLam _ b -> substituteTypeInNormal t' 0 b
        LCNormNeut nt  -> LCNormNeut $ nt `LCNeutTApp` t'
      where
        t' = substDefTypeInType dv p t

shift :: (LCTerm, Int, Int) -> LCTerm
shift (v, r, s) = go 0 0 v
  where
    go _ n (LCVar x)    = LCVar $ if x < n then x else x + s
    go m n (LCLam t b)  = LCLam (shiftTypeMin m (t, r)) $ go m (n + 1) b
    go m n (LCApp f a)  = go m n f `LCApp` go m n a
    go m n (LCTLam k b) = LCTLam k $ go m n b
    go m n (LCTApp f t) = go m n f `LCTApp` shiftTypeMin m (t, r)

shiftType :: (LCType, Int) -> LCType
shiftType = shiftTypeMin 0

shiftTypeMin :: Int -> (LCType, Int) -> LCType
shiftTypeMin m' (v, r) = go m' v
  where
    go _ LCBase       = LCBase
    go m (LCTVar p)   = LCTVar $ if p < m then p else p + r
    go m (LCArr a b)  = go m a `LCArr` go m b
    go m (LCUniv k a) = LCUniv k $ go (m + 1) a
    go m (LCTTLam k b) = LCTTLam k $ go (m + 1) b
    go m (LCTTApp f a) = go m f `LCTTApp` go m a

shiftValue :: (LCValue, Int, Int) -> LCTerm
shiftValue (v, r, s) = shift (liftLCValue v, r, s)

shiftNormal :: (LCNormalTerm, Int, Int) -> LCNormalTerm
shiftNormal = shiftNormalMin 0 0

shiftNormalMin :: Int -> Int -> (LCNormalTerm, Int, Int) -> LCNormalTerm
shiftNormalMin m' n' (v, r, s) = go m' n' v
  where
    go m n (LCNormLam t b)  = LCNormLam (shiftTypeMin m (t, r)) $ go m (n + 1) b
    go m n (LCNormTLam k b) = LCNormTLam k $ go (m + 1) n b
    go m n (LCNormNeut nt)  = LCNormNeut $ shiftNeutralMin m n (nt, r, s)

shiftNeutralMin :: Int -> Int -> (LCNeutralTerm, Int, Int) -> LCNeutralTerm
shiftNeutralMin m n (v, r, s) = go v
  where
    go (LCNeutVar x)    = LCNeutVar $ if x < n then x else x + s
    go (LCNeutApp f a)  = go f `LCNeutApp` shiftNormalMin m n (a, r, s)
    go (LCNeutTApp f t) = go f `LCNeutTApp` shiftTypeMin m (t, r)
