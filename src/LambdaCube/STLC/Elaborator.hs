module LambdaCube.STLC.Elaborator where

import           Data.List           (elemIndex)
import qualified Data.Text           as Text
import           LambdaCube.STLC.Ast

elaborateType :: ExtLCType -> LCType
elaborateType = go
  where
    go ExtLCBase = LCBase
    go (ExtLCArr a b) = go a `LCArr` go b
    go (ExtLCMTVar _) = error "invalid TemplateHaskell code splicer"

elaborate :: ExtLCTerm -> LCTerm
elaborate = go []
  where
    go l (ExtLCVar v)
      | Just idx <- v `elemIndex` l
      = LCVar idx
      | otherwise
      = error $ "Variable " <> Text.unpack v <> " is not in scope"
    go l (ExtLCLam v t b) = LCLam (elaborateType t) $ go (v : l) b
    go l (ExtLCApp f a) = go l f `LCApp` go l a
    go _ (ExtLCMVar _) = error "invalid TemplateHaskell code splicer"
