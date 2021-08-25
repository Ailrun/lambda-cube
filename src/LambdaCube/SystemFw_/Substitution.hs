{-# LANGUAGE ViewPatterns #-}
module LambdaCube.SystemFw_.Substitution
  ( substituteTypeInType
  , substituteValue
  , substituteNormalInNormal
  ) where

import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.Lifter

substituteTypeInType :: LCType -> Int -> LCType -> LCType
substituteTypeInType v = substDefTypeInType (v, 0)

substituteValue :: LCValue -> Int -> LCTerm -> LCTerm
substituteValue v = substDefValue (v, 0)

substituteNormalInNormal :: LCNormalTerm -> Int -> LCNormalTerm -> LCNormalTerm
substituteNormalInNormal v = substDefNormalInNormal (v, 0)

substDefTypeInType :: (LCType, Int) -> Int -> LCType -> LCType
substDefTypeInType = go
  where
    go _      _ LCBase                     = LCBase
    go dv     p (LCTVar ((== p) -> True))  = shiftType dv
    go _      p e@(LCTVar ((< p) -> True)) = e
    go _      _ (LCTVar q)                 = LCTVar $ q - 1
    go dv     p (LCArr a b)                = go dv p a `LCArr` go dv p b
    go (v, r) p (LCTTLam k b)              = LCTTLam k $ go (v, r + 1) (p + 1) b
    go dv     p (LCTTApp f a)              = go dv p f `LCTTApp` go dv p a

substDefValue :: (LCValue, Int) -> Int -> LCTerm -> LCTerm
substDefValue = go
  where
    go dv     x (LCVar ((== x) -> True))  = shiftValue dv
    go _      x e@(LCVar ((< x) -> True)) = e
    go _      _ (LCVar y)                 = LCVar $ y - 1
    go (v, s) x (LCLam t b)               = LCLam t $ go (v, s + 1) (x + 1) b
    go dv     x (LCApp f a)               = go dv x f `LCApp` go dv x a

substDefNormalInNormal :: (LCNormalTerm, Int) -> Int -> LCNormalTerm -> LCNormalTerm
substDefNormalInNormal = go
  where
    go (v, s) x (LCNormLam t b) = LCNormLam t $ go (v, s + 1) (x + 1) b
    go dv     x (LCNormNeut nt) = substDefNormalInNeutral dv x nt

substDefNormalInNeutral :: (LCNormalTerm, Int) -> Int -> LCNeutralTerm -> LCNormalTerm
substDefNormalInNeutral dv x = go
  where
    go (LCNeutVar ((== x) -> True)) = shiftNormal dv
    go e@(LCNeutVar ((< x) -> True)) = LCNormNeut e
    go (LCNeutVar y) = LCNormNeut . LCNeutVar $ y - 1
    go (LCNeutApp f a) =
      case go f of
        LCNormLam _ b -> substituteNormalInNormal a' 0 b
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = substDefNormalInNormal dv x a

shift :: (LCTerm, Int) -> LCTerm
shift (v, s) = go 0 v
  where
    go n (LCVar x)   = LCVar $ if x < n then x else x + s
    go n (LCLam t b) = LCLam t $ go (n + 1) b
    go n (LCApp f a) = go n f `LCApp` go n a

shiftType :: (LCType, Int) -> LCType
shiftType = shiftTypeMin 0

shiftTypeMin :: Int -> (LCType, Int) -> LCType
shiftTypeMin m' (v, r) = go m' v
  where
    go _ LCBase        = LCBase
    go m (LCTVar p)    = LCTVar $ if p < m then p else p + r
    go m (LCArr a b)   = go m a `LCArr` go m b
    go m (LCTTLam k b) = LCTTLam k $ go (m + 1) b
    go m (LCTTApp f a) = go m f `LCTTApp` go m a

shiftValue :: (LCValue, Int) -> LCTerm
shiftValue (v, s) = shift (liftLCValue v, s)

shiftNormal :: (LCNormalTerm, Int) -> LCNormalTerm
shiftNormal = shiftNormalMin 0

shiftNormalMin :: Int -> (LCNormalTerm, Int) -> LCNormalTerm
shiftNormalMin n' (v, s) = go n' v
  where
    go n (LCNormLam t b) = LCNormLam t $ go (n + 1) b
    go n (LCNormNeut nt) = LCNormNeut $ shiftNeutralMin n (nt, s)

shiftNeutralMin :: Int -> (LCNeutralTerm, Int) -> LCNeutralTerm
shiftNeutralMin n (v, s) = go v
  where
    go (LCNeutVar x)   = LCNeutVar $ if x < n then x else x + s
    go (LCNeutApp f a) = go f `LCNeutApp` shiftNormalMin n (a, s)
