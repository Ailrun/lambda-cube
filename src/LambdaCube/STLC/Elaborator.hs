module LambdaCube.STLC.Elaborator where

import           Data.List           (elemIndex)
import qualified Data.Text           as Text
import           LambdaCube.STLC.Ast

elaborate :: ExtLCTerm -> Either String LCTerm
elaborate = go []
  where
    go l (ExtLCVar v)
      | Just idx <- v `elemIndex` l
      = Right $ LCVar idx
      | otherwise
      = Left $ "Variable " <> Text.unpack v <> " is not in scope"
    go l (ExtLCLam v t b) = LCLam t <$> go (v : l) b
    go l (ExtLCApp f a) = LCApp <$> go l f <*> go l a
