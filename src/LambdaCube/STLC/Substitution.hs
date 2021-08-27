{-# LANGUAGE ViewPatterns #-}
module LambdaCube.STLC.Substitution
  ( substituteValue
  , substituteNormalInNormal
  ) where

import           LambdaCube.STLC.Ast
import           LambdaCube.STLC.Lifter

substituteValue :: LCValue -> Int -> LCTerm -> LCTerm
substituteValue v = substDefValue (v, 0)

substituteNormalInNormal :: LCNormalTerm -> Int -> LCNormalTerm -> LCNormalTerm
substituteNormalInNormal v = substDefNormalInNormal (v, 0)

substDefValue :: (LCValue, Int) -> Int -> LCTerm -> LCTerm
substDefValue = go
  where
    go dv     x (LCVar ((== x) -> True))  = shiftValue dv
    go _      x e@(LCVar ((< x) -> True)) = e
    go _      _ (LCVar y)                 = LCVar $ y - 1
    go _      _ (LCGlobal y)              = LCGlobal y
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

shiftTerm :: (LCTerm, Int) -> LCTerm
shiftTerm = shiftTermMin 0

shiftTermMin :: Int -> (LCTerm, Int) -> LCTerm
shiftTermMin n' (v, s) = go n' v
  where
    go n (LCVar x)    = LCVar $ if x < n then x else x + s
    go _ (LCGlobal x) = LCGlobal x
    go n (LCLam t b)  = LCLam t $ go (n + 1) b
    go n (LCApp f a)  = go n f `LCApp` go n a

shiftValue :: (LCValue, Int) -> LCTerm
shiftValue (v, s) = shiftTerm (liftLCValue v, s)

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
