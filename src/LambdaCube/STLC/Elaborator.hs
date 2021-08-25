module LambdaCube.STLC.Elaborator
  ( elaborate
  , elaborateType
  ) where

import           Data.List           (elemIndex)
import qualified Data.Text           as Text
import           LambdaCube.STLC.Ast

elaborate :: ExtLCTerm -> LCTerm
elaborate = go []
  where
    go l (ExtLCVar x)
      | Just idx <- x `elemIndex` l
      = LCVar idx
      | otherwise
      = error $ "Variable " <> Text.unpack x <> " is not in scope"
    go l (ExtLCLam x t b) = LCLam (elaborateType t) $ go (x : l) b
    go l (ExtLCApp f a) = go l f `LCApp` go l a
    go _ (ExtLCMVar _) = error "invalid TemplateHaskell code splicer"

elaborateType :: ExtLCType -> LCType
elaborateType = go
  where
    go ExtLCBase = LCBase
    go (ExtLCArr a b) = go a `LCArr` go b
    go (ExtLCMTVar _) = error "invalid TemplateHaskell code splicer"
