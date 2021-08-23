{- |
  TODO: Use real pretty printer library
-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.SystemFw.PrettyPrinter where

import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           LambdaCube.Common.PrettyPrinter
import           LambdaCube.SystemFw.Ast

prettyUnnamedKind :: LCKind -> Text
prettyUnnamedKind = prettyUnnamedKindPrec 0

prettyUnnamedType :: LCType -> Text
prettyUnnamedType = prettyUnnamedTypePrec 0

prettyUnnamedTerm :: LCTerm -> Text
prettyUnnamedTerm = prettyUnnamedTermPrec 0

prettyUnnamedKindPrec :: Int -> LCKind -> Text
prettyUnnamedKindPrec = go
  where
    go _ LCStar       = "*"
    go p (LCKArr a b) = wrapIfSpaced (p > 0) [go 1 a, "->", go 0 b]

prettyUnnamedTypePrec :: Int -> LCType -> Text
prettyUnnamedTypePrec = go
  where
    pKP = prettyUnnamedKindPrec

    go _ LCBase        = "#"
    go _ (LCTVar i)    = Text.pack $ show i
    go p (LCArr a b)   = wrapIfSpaced (p > 0) [go 1 a, "->", go 0 b]
    go p (LCUniv k b)  = wrapIfSpaced (p > 0) ["! :", pKP 0 k, ",", go 0 b]
    go p (LCTTLam k b) = wrapIfSpaced (p > 0) ["\\ :", pKP 0 k, ".", go 0 b]
    go p (LCTTApp f a) = wrapIfSpaced (p > 1) [go 1 f, go 2 a]

prettyUnnamedTermPrec :: Int -> LCTerm -> Text
prettyUnnamedTermPrec = go
  where
    pKP = prettyUnnamedKindPrec
    pTP = prettyUnnamedTypePrec

    go _ (LCVar i)    = Text.pack $ show i
    go p (LCLam t b)  = wrapIfSpaced (p > 0) ["\\ :", pTP 0 t, ".", go 0 b]
    go p (LCApp f a)  = wrapIfSpaced (p > 1) [go 1 f, go 2 a]
    go p (LCTLam k b) = wrapIfSpaced (p > 0) ["@\\ :", pKP 0 k, ".", go 0 b]
    go p (LCTApp f t) = wrapIfSpaced (p > 1) [go 1 f, "@" <> pTP 1 t]
