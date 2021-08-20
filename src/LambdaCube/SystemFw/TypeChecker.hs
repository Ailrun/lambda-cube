module LambdaCube.SystemFw.TypeChecker where

import           Data.List               (uncons)
import           LambdaCube.SystemFw.Ast

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
    go tl (LCTLam k b) = LCUniv k <$> go tl b
    go tl (LCTApp f t)
      | Just (LCUniv tk' rt) <- go tl f
      , Just tk <- inferKind t
      , tk == tk'
      = Just $ substituteTypeInType 0 t rt
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
    go kl (LCUniv k a) = go (k : kl) a
    go kl (LCTTLam k b) = LCKArr k <$> go (k : kl) b
    go kl (LCTTApp f a)
      | Just (LCKArr ak' rk) <- go kl f
      , Just ak <- go kl a
      , ak == ak'
      = Just rk
      | otherwise
      = Nothing
