module LambdaCube.SystemF.TypeChecker where

import           Data.List                       (uncons)
import           LambdaCube.SystemF.Ast
import           LambdaCube.SystemF.Substitution

infer :: LCTerm -> LCType
infer = go []
  where
    go tl (LCVar n) = maybe (error "Out-of-scope variable") fst . uncons $ drop n tl
    go tl (LCLam t b) = t `LCArr` go (t : tl) b
    go tl (LCApp f a)
      | LCArr at rt <- go tl f
      , at == go tl a
      = rt
      | otherwise
      = error "Function argument type mismatch"
    go tl (LCTLam b) = LCUniv $ go tl b
    go tl (LCTApp f t)
      | LCUniv rt <- go tl f
      = substituteTypeInType 0 t rt
      | otherwise
      = error "Function argument type mismatch"
