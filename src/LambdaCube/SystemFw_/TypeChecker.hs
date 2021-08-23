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

infer :: LCTerm -> Maybe LCType
infer = go []
  where
    go tl (LCVar n) = fmap fst . uncons $ drop n tl
    go tl (LCLam t b)
      | Just LCStar <- inferKind t
      = LCArr v <$> go (v : tl) b
      | otherwise
      = Nothing
      where
        v = reduceType t
    go tl (LCApp f a)
      | Just (LCArr at' rt) <- go tl f
      , Just at <- go tl a
      , at == at'
      = Just rt
      | otherwise
      = Nothing

inferKind :: LCType -> Maybe LCKind
inferKind = go []
  where
    go _  LCBase = Just LCStar
    go kl (LCTVar n) = fmap fst . uncons $ drop n kl
    go kl (LCArr a b)
      | Just LCStar <- go kl a
      , Just LCStar <- go kl b
      = Just LCStar
      | otherwise
      = Nothing
    go kl (LCTTLam k b) = LCKArr k <$> go (k : kl) b
    go kl (LCTTApp f a)
      | Just (LCKArr ak' rk) <- go kl f
      , Just ak <- go kl a
      , ak == ak'
      = Just rk
      | otherwise
      = Nothing
