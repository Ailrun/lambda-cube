module LambdaCube.SystemF.Normalizer where

import           LambdaCube.SystemF.Ast
import           LambdaCube.SystemF.TypeChecker (substituteTypeInType)

substituteNormal :: Int -> LCNormalTerm -> LCNormalTerm -> LCNormalTerm
substituteNormal n v = go n
  where
    go m (LCNormLam t b) = LCNormLam t $ go (m + 1) b
    go m (LCNormTLam b)  = LCNormTLam $ go m b
    go m (LCNormNeut nt) = substituteNeutral m v nt

substituteNeutral :: Int -> LCNormalTerm -> LCNeutralTerm -> LCNormalTerm
substituteNeutral n v = go n
  where
    go m e@(LCNeutVar l)
      | m == l = v
      | otherwise = LCNormNeut e
    go m (LCNeutApp f a) =
      case go m f of
        LCNormLam _ b -> substituteNormal 0 a' b
        LCNormTLam _  -> error "Did you really type check this?"
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = substituteNormal m v a
    go m (LCNeutTApp f t) =
      case go m f of
        LCNormLam _ _ -> error "Did you really type check this?"
        LCNormTLam b  -> substituteTypeNormal 0 t b
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutTApp` t

substituteTypeNormal :: Int -> LCType -> LCNormalTerm -> LCNormalTerm
substituteTypeNormal n v = go n
  where
    go m (LCNormLam t b) = LCNormLam (substituteTypeInType m v t) $ go m b
    go m (LCNormTLam b)  = LCNormTLam $ go (m + 1) b
    go m (LCNormNeut nt) = substituteTypeNeutral m v nt

substituteTypeNeutral :: Int -> LCType -> LCNeutralTerm -> LCNormalTerm
substituteTypeNeutral n v = go n
  where
    go _ e@(LCNeutVar _) = LCNormNeut e
    go m (LCNeutApp f a) =
      case go m f of
        LCNormLam _ b -> substituteNormal 0 a' b
        LCNormTLam _  -> error "Did you really type check this?"
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = substituteTypeNormal m v a
    go m (LCNeutTApp f t) =
      case go m f of
        LCNormLam _ _ -> error "Did you really type check this?"
        LCNormTLam b  -> substituteTypeNormal 0 t' b
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutTApp` t'
      where
        t' = substituteTypeInType m v t

normalize :: LCTerm -> LCNormalTerm
normalize = go
  where
    go (LCVar n) = LCNormNeut $ LCNeutVar n
    go (LCLam t b) = LCNormLam t $ go b
    go (LCTLam b) = LCNormTLam $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam _ b   -> substituteNormal 0 a' b
        LCNormTLam _    -> error "Did you really type check this?"
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutApp` a'
      where
        a' = go a
    go (LCTApp f t) =
      case go f of
        LCNormLam _ _   -> error "Did you really type check this?"
        LCNormTLam b    -> substituteTypeNormal 0 t b
        LCNormNeut neut -> LCNormNeut $ neut `LCNeutTApp` t
