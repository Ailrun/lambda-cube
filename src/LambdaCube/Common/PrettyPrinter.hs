{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.Common.PrettyPrinter where

import           Data.Text (Text)
import qualified Data.Text as Text

wrapIfSpaced :: Bool -> [Text] -> Text
wrapIfSpaced b = wrapIf b . spaced

wrap :: Text -> Text
wrap t = "(" <> t <> ")"

wrapIf :: Bool -> Text -> Text
wrapIf True  = wrap
wrapIf False = id

spaced :: [Text] -> Text
spaced = Text.intercalate " "
