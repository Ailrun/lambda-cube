{- |
  TODO: Use real pretty printer library
-}
{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.STLC.PrettyPrinter where

import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           LambdaCube.Common.PrettyPrinter
import           LambdaCube.STLC.Ast

prettyType :: LCType -> Text
prettyType = prettyTypePrec 0

prettyTerm :: LCTerm -> Text
prettyTerm = prettyTermPrec 0

prettyTypePrec :: Int -> LCType -> Text
prettyTypePrec _ LCBase = "#"
prettyTypePrec p (LCArr a b) = prettyWrapIf (p > 0) $ prettyTypePrec 1 a <> " -> " <> prettyTypePrec 0 b

prettyTermPrec :: Int -> LCTerm -> Text
prettyTermPrec _ (LCVar n) = Text.pack $ show n
prettyTermPrec p (LCLam t b) = prettyWrapIf (p > 0) $ "\\ " <> prettyTypePrec 0 t <> " . " <> prettyTermPrec 0 b
prettyTermPrec p (LCApp f a) = prettyWrapIf (p > 1) $ prettyTermPrec 1 f <> " " <> prettyTermPrec 2 a
