{- |
  TODO: Use real pretty printer library
-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.SystemFw_.PrettyPrinter
  ( prettyUnnamedTerm
  , prettyShowUnnamedTerm
  , prettyUnnamedType
  , prettyShowUnnamedType
  , prettyUnnamedKind
  , prettyShowUnnamedKind
  ) where

import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           LambdaCube.Common.PrettyPrinter
import           LambdaCube.SystemFw_.Ast

prettyUnnamedTerm :: LCTerm -> Text
prettyUnnamedTerm = prettyUnnamedTermPrec 0

prettyShowUnnamedTerm :: LCTerm -> String
prettyShowUnnamedTerm = Text.unpack . prettyUnnamedTerm

prettyUnnamedType :: LCType -> Text
prettyUnnamedType = prettyUnnamedTypePrec 0

prettyShowUnnamedType :: LCType -> String
prettyShowUnnamedType = Text.unpack . prettyUnnamedType

prettyUnnamedKind :: LCKind -> Text
prettyUnnamedKind = prettyUnnamedKindPrec 0

prettyShowUnnamedKind :: LCKind -> String
prettyShowUnnamedKind = Text.unpack . prettyUnnamedKind

prettyUnnamedTermPrec :: Int -> LCTerm -> Text
prettyUnnamedTermPrec = go
  where
    pTP = prettyUnnamedTypePrec

    go _ (LCVar x)   = Text.pack $ show x
    go p (LCLam t b) = wrapIfSpaced (p > 0) ["\\ :", pTP 0 t, ".", go 0 b]
    go p (LCApp f a) = wrapIfSpaced (p > 1) [go 1 f, go 2 a]

prettyUnnamedTypePrec :: Int -> LCType -> Text
prettyUnnamedTypePrec = go
  where
    pKP = prettyUnnamedKindPrec

    go _ LCBase        = "#"
    go _ (LCTVar p)    = Text.pack $ show p
    go p (LCArr a b)   = wrapIfSpaced (p > 0) [go 1 a, "->", go 0 b]
    go p (LCTTLam k b) = wrapIfSpaced (p > 0) ["\\ :", pKP 0 k, ".", go 0 b]
    go p (LCTTApp f a) = wrapIfSpaced (p > 1) [go 1 f, go 2 a]

prettyUnnamedKindPrec :: Int -> LCKind -> Text
prettyUnnamedKindPrec = go
  where
    go _ LCStar       = "*"
    go p (LCKArr a b) = wrapIfSpaced (p > 0) [go 1 a, "->", go 0 b]
