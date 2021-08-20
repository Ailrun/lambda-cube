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
prettyTypePrec _ LCBase = "#"
prettyTypePrec _ (LCTVar i) = Text.pack $ show i
prettyTypePrec p (LCArr a b) = prettyWrapIf (p > 1) $ prettyTypePrec 2 a <> " -> " <> prettyTypePrec 1 b
prettyTypePrec p (LCUniv b) = prettyWrapIf (p > 0) $ "! , " <> prettyTypePrec 0 b

prettyTermPrec :: Int -> LCTerm -> Text
prettyTermPrec _ (LCVar i) = Text.pack $ show i
prettyTermPrec p (LCLam t b) = prettyWrapIf (p > 0) $ "\\ " <> prettyTypePrec 0 t <> " . " <> prettyTermPrec 0 b
prettyTermPrec p (LCApp f a) = prettyWrapIf (p > 1) $ prettyTermPrec 1 f <> " " <> prettyTermPrec 2 a
prettyTermPrec p (LCTLam b) = prettyWrapIf (p > 0) $ "@\\ . " <> prettyTermPrec 0 b
prettyTermPrec p (LCTApp f t) = prettyWrapIf (p > 1) $ prettyTermPrec 1 f <> " @" <> prettyTypePrec 2 t
