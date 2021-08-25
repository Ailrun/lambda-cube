module LambdaCube.SystemF.TypeChecker
  ( infer
  ) where

import           Data.List                       (uncons)
import           LambdaCube.SystemF.Ast
import           LambdaCube.SystemF.Substitution

infer :: LCTerm -> LCType
infer = go []
  where
    go tl (LCVar x) = maybe (error "Out-of-scope variable") fst . uncons $ drop x tl
    go tl (LCLam t b) = t `LCArr` go (t : tl) b
    go tl (LCApp f a)
      | LCArr at rt <- go tl f
      , at == go tl a
      = rt
      | otherwise
      = error "Function argument type mismatch"
    go tl (LCTLam b) = LCUniv $ go (fmap (shiftType . (, 1)) tl) b
    go tl (LCTApp f t)
      | LCUniv rt <- go tl f
      = substituteTypeInType t 0 rt
      | otherwise
      = error "Function argument type mismatch"
