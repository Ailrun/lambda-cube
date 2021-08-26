module LambdaCube.SystemFw.TypeChecker
  ( reduceType

  , infer
  , inferKind
  ) where

import           Data.List                        (uncons)
import           LambdaCube.SystemFw.Ast
import           LambdaCube.SystemFw.Substitution

reduceType :: LCType -> LCType
reduceType = go
  where
    go LCBase = LCBase
    go e@(LCTVar _) = e
    go (LCArr a b) = go a `LCArr` go b
    go (LCUniv k a) = LCUniv k $ go a
    go (LCTTLam k b) = LCTTLam k $ go b
    go (LCTTApp f a)
      | LCTTLam _ b <- go f
      , v <- go a
      = go $ substituteTypeInType v 0 b
      | otherwise
      = error "Did you really kind check this?"

infer :: LCTerm -> LCType
infer = go [] []
  where
    go _  tl (LCVar n) = maybe (error "Out-of-scope variable") fst . uncons $ drop n tl
    go kl tl (LCLam t b)
      | LCStar <- inferKind kl t
      = v `LCArr` go kl (v : tl) b
      | otherwise
      = error "Function argument kind mismatch"
      where
        v = reduceType t
    go kl tl (LCApp f a)
      | LCArr at rt <- go kl tl f
      , at == go kl tl a
      = rt
      | otherwise
      = error "Function argument type mismatch"
    go kl tl (LCTLam k b) = LCUniv k $ go (k : kl) (fmap (shiftType . (, 1)) tl) b
    go kl tl (LCTApp f t)
      | LCUniv tk rt <- go kl tl f
      , tk == inferKind kl t
      = substituteTypeInType v 0 rt
      | otherwise
      = error "Function argument kind mismatch"
      where
        v = reduceType t

inferKind :: [LCKind] -> LCType -> LCKind
inferKind = go
  where
    go _  LCBase = LCStar
    go kl (LCTVar n) = maybe (error "Out-of-scope variable") fst . uncons $ drop n kl
    go kl (LCArr a b)
      | LCStar <- go kl a
      , LCStar <- go kl b
      = LCStar
      | otherwise
      = error "Arrow kind mismatch"
    go kl (LCUniv k a) = go (k : kl) a
    go kl (LCTTLam k b) = LCKArr k $ go (k : kl) b
    go kl (LCTTApp f a)
      | LCKArr ak rk <- go kl f
      , ak == go kl a
      = rk
      | otherwise
      = error "Function argument kind mismatch"
