module LambdaCube.SystemFw_.Elaborator where

import           Data.List                (elemIndex)
import qualified Data.Text                as Text
import           LambdaCube.SystemFw_.Ast

elaborate :: ExtLCTerm -> Either String LCTerm
elaborate = go []
  where
    go l (ExtLCVar x)
      | Just n <- x `elemIndex` l
      = Right $ LCVar n
      | otherwise
      = Left $ "Term variable " <> Text.unpack x <> " is not in scope"
    go l (ExtLCLam x t b) = LCLam <$> elaborateType t <*> go (x : l) b
    go l (ExtLCApp f a) = LCApp <$> go l f <*> go l a

elaborateType :: ExtLCType -> Either String LCType
elaborateType = go []
  where
    go _ ExtLCBase = Right LCBase
    go l (ExtLCTVar x)
      | Just n <- x `elemIndex` l
      = Right $ LCTVar n
      | otherwise
      = Left  $ "Type variable " <> Text.unpack x <> " is not in scope"
    go l (ExtLCArr a b) = LCArr <$> go l a <*> go l b
    go l (ExtLCTTLam x k b) = LCTTLam k <$> go (x : l) b
    go l (ExtLCTTApp f a) = LCTTApp <$> go l f <*> go l a
