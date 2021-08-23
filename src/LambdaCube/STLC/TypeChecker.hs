module LambdaCube.STLC.TypeChecker where

import           Data.List           (uncons)
import           LambdaCube.STLC.Ast

infer :: LCTerm -> LCType
infer = go []
  where
    go l (LCVar n) = maybe (error "Out-of-scope variable") fst . uncons $ drop n l
    go l (LCLam t b) = t `LCArr` go (t : l) b
    go l (LCApp f a)
      | LCArr at rt <- go l f
      , at == go l a
      = rt
      | otherwise
      = error "Function argument type mismatch"
