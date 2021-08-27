{-# LANGUAGE DuplicateRecordFields #-}
module LambdaCube.STLC.TypeChecker
  ( checkModule
  , inferType
  ) where

import           Data.List               (mapAccumL, uncons)
import           Data.Maybe              (catMaybes, fromJust)
import           LambdaCube.Common.Error
import           LambdaCube.STLC.Ast

checkModule :: LCModule -> [(Identifier, LCType)]
checkModule = catMaybes . snd . mapAccumL go [] . (getModuleDecls :: LCModule -> [LCModuleDecl])
  where
    go ge (LCModuleTermDecl x b) = ((x, t) : ge, Just (x, t))
      where
        t = inferTypeUnder ge b
    go ge (LCModuleTypeDecl _ _) = (ge, Nothing)

inferType :: LCTerm -> LCType
inferType = inferTypeUnder []

inferTypeUnder :: [(Identifier, LCType)] -> LCTerm -> LCType
inferTypeUnder ge = go []
  where
    go l (LCVar x) = maybe elaborationBug fst . uncons $ drop x l
    go _ (LCGlobal x) = fromJust elaborationBug $ lookup x ge
    go l (LCLam t b) = t `LCArr` go (t : l) b
    go l (LCApp f a)
      | LCArr at rt <- go l f
      , at == go l a
      = rt
      | otherwise
      = argumentTypeError
