{-# LANGUAGE DuplicateRecordFields #-}
module LambdaCube.STLC.Evaluator
  ( evaluateTermWith
  , evaluateTerm
  ) where

import           Data.Maybe                   (mapMaybe)
import           LambdaCube.Common.Error
import           LambdaCube.STLC.Ast
import           LambdaCube.STLC.Substitution

evaluateTermWith :: LCModule -> LCTerm -> LCValue
evaluateTermWith = evaluateTermUnder . mapMaybe go . (getModuleDecls :: LCModule -> [LCModuleDecl])
  where
    go (LCModuleTermDecl x b) = Just (x, b)
    go (LCModuleTypeDecl _ _) = Nothing

evaluateTerm :: LCTerm -> LCValue
evaluateTerm = evaluateTermUnder []

evaluateTermUnder :: [(Identifier, LCTerm)] -> LCTerm -> LCValue
evaluateTermUnder ge = go
  where
    go (LCVar _) = notTypeChecked
    go (LCGlobal x) = maybe elaborationBug evaluateTerm $ lookup x ge
    go (LCLam t b) = LCValLam t b
    go (LCApp f a)
      | LCValLam _ b <- go f
      , v <- go a
      = go $ substituteValue v 0 b
