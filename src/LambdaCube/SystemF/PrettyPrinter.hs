{- |
  TODO: Use real pretty printer library
-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.SystemF.PrettyPrinter
  ( prettyUnnamedTerm
  , prettyUnnamedType
  ) where

import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           LambdaCube.Common.PrettyPrinter
import           LambdaCube.SystemF.Ast

prettyUnnamedTerm :: LCTerm -> Text
prettyUnnamedTerm = prettyUnnamedTermPrec 0

prettyUnnamedType :: LCType -> Text
prettyUnnamedType = prettyUnnamedTypePrec 0

prettyUnnamedTermPrec :: Int -> LCTerm -> Text
prettyUnnamedTermPrec = go
  where
    pTP = prettyUnnamedTypePrec

    go _ (LCVar x)    = Text.pack $ show x
    go p (LCLam t b)  = wrapIfSpaced (p > 0) ["\\ :", pTP 0 t, ".", go 0 b]
    go p (LCApp f a)  = wrapIfSpaced (p > 1) [go 1 f, go 2 a]
    go p (LCTLam b)   = wrapIfSpaced (p > 0) ["@\\ : * .", go 0 b]
    go p (LCTApp f t) = wrapIfSpaced (p > 1) [go 1 f, "@" <> pTP 1 t]

prettyUnnamedTypePrec :: Int -> LCType -> Text
prettyUnnamedTypePrec = go
  where
    go _ LCBase      = "#"
    go _ (LCTVar p)  = Text.pack $ show p
    go p (LCArr a b) = wrapIfSpaced (p > 0) [go 1 a, "->", go 0 b]
    go p (LCUniv b)  = wrapIfSpaced (p > 0) ["! : * ,", go 0 b]
