{- |
  TODO: Use real pretty printer library
-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.SystemF.PrettyPrinter where

import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           LambdaCube.Common.PrettyPrinter
import           LambdaCube.SystemF.Ast

prettyType :: LCType -> Text
prettyType = prettyTypePrec 0

prettyTerm :: LCTerm -> Text
prettyTerm = prettyTermPrec 0

prettyTypePrec :: Int -> LCType -> Text
prettyTypePrec = go
  where
    go _ LCBase      = "#"
    go _ (LCTVar i)  = Text.pack $ show i
    go p (LCArr a b) = wrapIfSpaced (p > 0) [go 1 a, "->", go 0 b]
    go p (LCUniv b)  = wrapIfSpaced (p > 0) ["! : * ,", go 0 b]

prettyTermPrec :: Int -> LCTerm -> Text
prettyTermPrec = go
  where
    pTP = prettyTypePrec

    go _ (LCVar i)    = Text.pack $ show i
    go p (LCLam t b)  = wrapIfSpaced (p > 0) ["\\ :", pTP 0 t, ".", go 0 b]
    go p (LCApp f a)  = wrapIfSpaced (p > 1) [go 1 f, go 2 a]
    go p (LCTLam b)   = wrapIfSpaced (p > 0) ["@\\ : * .", go 0 b]
    go p (LCTApp f t) = wrapIfSpaced (p > 1) [go 1 f, "@" <> pTP 1 t]
