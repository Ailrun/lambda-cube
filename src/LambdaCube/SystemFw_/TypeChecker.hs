module LambdaCube.SystemFw_.TypeChecker where

import           Data.List                         (uncons)
import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.Substitution

reduceType :: LCType -> LCType
reduceType = go
  where
    go LCBase = LCBase
    go e@(LCTVar _) = e
    go (LCArr a b) = go a `LCArr` go b
    go (LCTTLam k b) =  LCTTLam k $ go b
    go (LCTTApp f a)
      | LCTTLam _ b <- go f
      , v <- go a
      = go $ substituteTypeInType 0 v b
      | otherwise
      = error "Did you really kind check this?"

infer :: LCTerm -> LCType
infer = go []
  where
    go tl (LCVar n) = maybe (error "Out-of-scope variable") fst . uncons $ drop n tl
    go tl (LCLam t b)
      | LCStar <- inferKind t
      = v `LCArr` go (v : tl) b
      | otherwise
      = error "Function argument kind mismatch"
      where
        v = reduceType t
    go tl (LCApp f a)
      | LCArr at rt <- go tl f
      , at == go tl a
      = rt
      | otherwise
      = error "Function argument type mismatch"

inferKind :: LCType -> LCKind
inferKind = go []
  where
    go _  LCBase = LCStar
    go kl (LCTVar n) = maybe (error "Out-of-scope variable") fst . uncons $ drop n kl
    go kl (LCArr a b)
      | LCStar <- go kl a
      , LCStar <- go kl b
      = LCStar
      | otherwise
      = error "Arrow kind mismatch"
    go kl (LCTTLam k b) = k `LCKArr` go (k : kl) b
    go kl (LCTTApp f a)
      | LCKArr ak rk <- go kl f
      , ak == go kl a
      = rk
      | otherwise
      = error "Function argument kind mismatch"
