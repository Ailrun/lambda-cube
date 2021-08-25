module LambdaCube.SystemFw_.Elaborator
  ( elaborate
  , elaborateType
  , elaborateKind
  ) where

import           Data.List                (elemIndex)
import qualified Data.Text                as Text
import           LambdaCube.SystemFw_.Ast

elaborate :: ExtLCTerm -> LCTerm
elaborate = go []
  where
    go l (ExtLCVar x)
      | Just n <- x `elemIndex` l
      = LCVar n
      | otherwise
      = error $ "Term variable " <> Text.unpack x <> " is not in scope"
    go l (ExtLCLam x t b) = LCLam (elaborateType t) $ go (x : l) b
    go l (ExtLCApp f a) = go l f `LCApp` go l a
    go _ (ExtLCMVar _) = error "invalid TemplateHaskell code splicer"

elaborateType :: ExtLCType -> LCType
elaborateType = go []
  where
    go _ ExtLCBase = LCBase
    go l (ExtLCTVar p)
      | Just n <- p `elemIndex` l
      = LCTVar n
      | otherwise
      = error $ "Type variable " <> Text.unpack p <> " is not in scope"
    go l (ExtLCArr a b) = go l a `LCArr` go l b
    go l (ExtLCTTLam p k b) = LCTTLam (elaborateKind k) $ go (p : l) b
    go l (ExtLCTTApp f a) = go l f `LCTTApp` go l a
    go _ (ExtLCMTVar _) = error "invalid TemplateHaskell code splicer"

elaborateKind :: ExtLCKind -> LCKind
elaborateKind = go
  where
    go ExtLCStar = LCStar
    go (ExtLCKArr a b) = go a `LCKArr` go b
    go (ExtLCMKVar _) = error "invalid TemplateHaskell code splicer"
