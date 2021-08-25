module LambdaCube.SystemFw.Elaborator
  ( elaborate
  , elaborateType
  , elaborateKind
  ) where

import           Data.List               (elemIndex)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           LambdaCube.SystemFw.Ast

elaborate :: ExtLCTerm -> LCTerm
elaborate = go [] []
  where
    go _  vl (ExtLCVar x)
      | Just n <- x `elemIndex` vl
      = LCVar n
      | otherwise
      = error $ "Term variable " <> Text.unpack x <> " is not in scope"
    go tl vl (ExtLCLam x t b) = LCLam (elaborateType tl t) $ go tl (x : vl) b
    go tl vl (ExtLCApp f a) = go tl vl f `LCApp` go tl vl a
    go tl vl (ExtLCTLam p k b) = LCTLam (elaborateKind k) $ go (p : tl) vl b
    go tl vl (ExtLCTApp f t) = go tl vl f `LCTApp` elaborateType tl t
    go _  _  (ExtLCMVar _) = error "invalid TemplateHaskell code splicer"

elaborateType :: [Text] -> ExtLCType -> LCType
elaborateType = go
  where
    go _ ExtLCBase = LCBase
    go l (ExtLCTVar p)
      | Just n <- p `elemIndex` l
      = LCTVar n
      | otherwise
      = error $ "Type variable " <> Text.unpack p <> " is not in scope"
    go l (ExtLCArr a b) = go l a `LCArr` go l b
    go l (ExtLCUniv p k a) = LCUniv (elaborateKind k) $ go (p : l) a
    go l (ExtLCTTLam p k b) = LCTTLam (elaborateKind k) $ go (p : l) b
    go l (ExtLCTTApp f a) = go l f `LCTTApp` go l a
    go _ (ExtLCMTVar _) = error "invalid TemplateHaskell code splicer"

elaborateKind :: ExtLCKind -> LCKind
elaborateKind = go
  where
    go ExtLCStar = LCStar
    go (ExtLCKArr a b) = go a `LCKArr` go b
    go (ExtLCMKVar _) = error "invalid TemplateHaskell code splicer"
