module LambdaCube.SystemF.TypeChecker where

import           Data.List              (uncons)
import           LambdaCube.SystemF.Ast

substituteType :: Int -> LCType -> LCTerm -> LCTerm
substituteType _ _ e@(LCVar _) = e
substituteType n v (LCLam t b) = LCLam (substituteTypeInType n v t) (substituteType n v b)
substituteType n v (LCApp f a) = LCApp (substituteType n v f) (substituteType n v a)
substituteType n v (LCTLam b) = LCTLam (substituteType (n + 1) v b)
substituteType n v (LCTApp f t) = LCTApp (substituteType n v f) (substituteTypeInType n v t)

substituteTypeInType :: Int -> LCType -> LCType -> LCType
substituteTypeInType _ _ LCBase = LCBase
substituteTypeInType n v e@(LCTVar m)
  | n == m = v
  | otherwise = e
substituteTypeInType n v (LCArr a b) = LCArr (substituteTypeInType n v a) (substituteTypeInType n v b)
substituteTypeInType n v (LCUniv a) = LCUniv (substituteTypeInType (n + 1) v a)

infer :: LCTerm -> Maybe LCType
infer = go []
  where
    go tl (LCVar n) = fmap fst . uncons $ drop n tl
    go tl (LCLam t b) = LCArr t <$> go (t : tl) b
    go tl (LCApp f a)
      | Just (LCArr at' rt) <- go tl f
      , Just at <- go tl a
      , at == at'
      = Just rt
      | otherwise
      = Nothing
    go tl (LCTLam b) = LCUniv <$> go tl b
    go tl (LCTApp f t)
      | Just (LCUniv rt) <- go tl f
      = Just (substituteTypeInType 0 t rt)
      | otherwise
      = Nothing
