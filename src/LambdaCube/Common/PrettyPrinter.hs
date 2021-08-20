{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.Common.PrettyPrinter where

import           Data.Text (Text)

prettyWrap :: Text -> Text
prettyWrap t = "(" <> t <> ")"

prettyWrapIf :: Bool -> Text -> Text
prettyWrapIf True  = prettyWrap
prettyWrapIf False = id
