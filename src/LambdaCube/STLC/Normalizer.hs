{-# LANGUAGE DuplicateRecordFields #-}
module LambdaCube.STLC.Normalizer
  ( normalizeTermWith
  , normalizeTerm
  ) where

import           Data.Maybe                   (mapMaybe)
import           LambdaCube.Common.Error
import           LambdaCube.STLC.Ast
import           LambdaCube.STLC.Substitution

normalizeTermWith :: LCModule -> LCTerm -> LCNormalTerm
normalizeTermWith = normalizeTermUnder . mapMaybe go . (getModuleDecls :: LCModule -> [LCModuleDecl])
  where
    go (LCModuleTermDecl x b) = Just (x, b)
    go (LCModuleTypeDecl _ _) = Nothing

normalizeTerm :: LCTerm -> LCNormalTerm
normalizeTerm = normalizeTermUnder []

normalizeTermUnder :: [(Identifier, LCTerm)] -> LCTerm -> LCNormalTerm
normalizeTermUnder ge = go
  where
    go (LCVar x) = LCNormNeut $ LCNeutVar x
    go (LCGlobal x) = maybe elaborationBug go $ lookup x ge
    go (LCLam t b) = LCNormLam t $ go b
    go (LCApp f a) =
      case go f of
        LCNormLam _ b -> substituteNormalInNormal a' 0 b
        LCNormNeut nt -> LCNormNeut $ nt `LCNeutApp` a'
      where
        a' = go a
