{-# LANGUAGE DuplicateRecordFields #-}
module LambdaCube.STLC.Elaborator
  ( elaborateModule
  , elaborateTerm
  , elaborateType
  ) where

import           Data.List               (elemIndex, mapAccumL)
import           LambdaCube.Common.Error
import           LambdaCube.STLC.Ast

elaborateModule :: ExtLCModule -> LCModule
elaborateModule (ExtLCModule ds) =
  LCModule
  . snd
  . mapAccumL go ([], [])
  $ concatMap unsplice ds
  where
    unsplice (ExtLCModuleSplice (ExtLCModule ds')) = concatMap unsplice ds'
    unsplice e                                     = [e]

    go (termPrev, typePrev) (ExtLCModuleTermDecl x b) =
      ((x : termPrev, typePrev), LCModuleTermDecl x (elaborateTermUnder termPrev typePrev b))
    go (termPrev, typePrev) (ExtLCModuleTypeDecl p t) =
      ((termPrev, p : typePrev), LCModuleTypeDecl p (elaborateTypeUnder typePrev t))
    go _                    (ExtLCModuleSplice _) =
      impossibleCase
elaborateModule (ExtLCMMVar m) = leftoverMetaVar m

elaborateTerm :: ExtLCTerm -> LCTerm
elaborateTerm = elaborateTermUnder [] []

elaborateType :: ExtLCType -> LCType
elaborateType = elaborateTypeUnder []

elaborateTermUnder :: [Identifier] -> [Identifier] -> ExtLCTerm -> LCTerm
elaborateTermUnder ge gt = go []
  where
    go l (ExtLCVar x)
      | x `elem` ge
      = LCGlobal x
      | Just idx <- x `elemIndex` l
      = LCVar idx
      | otherwise
      = termVarNotInScope x
    go l (ExtLCLam x t b) = LCLam (elaborateTypeUnder gt t) $ go (x : l) b
    go l (ExtLCApp f a) = go l f `LCApp` go l a
    go _ (ExtLCMVar h) = leftoverMetaVar h

elaborateTypeUnder :: [Identifier] -> ExtLCType -> LCType
elaborateTypeUnder gt = go
  where
    go ExtLCBase      = LCBase
    go (ExtLCTVar p)
      | p `elem` gt
      = LCTGlobal p
      | otherwise
      = typeVarNotInScope p
    go (ExtLCArr a b) = go a `LCArr` go b
    go (ExtLCMTVar h) = leftoverMetaVar h
