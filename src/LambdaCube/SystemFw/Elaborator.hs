module LambdaCube.SystemFw.Elaborator where

import           Data.List               (elemIndex)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           LambdaCube.SystemFw.Ast

elaborate :: ExtLCTerm -> Either String LCTerm
elaborate = go [] []
  where
    go _  vl (ExtLCVar x)
      | Just n <- x `elemIndex` vl
      = Right $ LCVar n
      | otherwise
      = Left $ "Term variable " <> Text.unpack x <> " is not in scope"
    go tl vl (ExtLCLam x t b) = LCLam <$> elaborateType tl t <*> go tl (x : vl) b
    go tl vl (ExtLCApp f a) = LCApp <$> go tl vl f <*> go tl vl a
    go tl vl (ExtLCTLam x k b) = LCTLam k <$> go (x : tl) vl b
    go tl vl (ExtLCTApp f t) = LCTApp <$> go tl vl f <*> elaborateType tl t

elaborateType :: [Text] -> ExtLCType -> Either String LCType
elaborateType = go
  where
    go _ ExtLCBase = Right LCBase
    go l (ExtLCTVar x)
      | Just n <- x `elemIndex` l
      = Right $ LCTVar n
      | otherwise
      = Left  $ "Type variable " <> Text.unpack x <> " is not in scope"
    go l (ExtLCArr a b) = LCArr <$> go l a <*> go l b
    go l (ExtLCUniv x k a) = LCUniv k <$> go (x : l) a
    go l (ExtLCTTLam x k b) = LCTTLam k <$> go (x : l) b
    go l (ExtLCTTApp f a) = LCTTApp <$> go l f <*> go l a
