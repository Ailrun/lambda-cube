module LambdaCube.SystemF.TypeChecker where

import           Data.List              (uncons)
import           LambdaCube.SystemF.Ast

substituteType :: Int -> LCType -> LCTerm -> LCTerm
substituteType n v = go n
  where
    go _ e@(LCVar _)  = e
    go m (LCLam t b)  = LCLam (substituteTypeInType m v t) $ go m b
    go m (LCApp f a)  = go m f `LCApp` go m a
    go m (LCTLam b)   = LCTLam $ go (m + 1) b
    go m (LCTApp f t) = go m f `LCTApp` substituteTypeInType m v t

substituteTypeInType :: Int -> LCType -> LCType -> LCType
substituteTypeInType n v = go n
  where
    go _ LCBase       = LCBase
    go m e@(LCTVar l) = if m == l then v else e
    go m (LCArr a b)  = go m a `LCArr` go m b
    go m (LCUniv a)   = LCUniv $ go (m + 1) a

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
      = Just $ substituteTypeInType 0 t rt
      | otherwise
      = Nothing
