{-# LANGUAGE OverloadedStrings #-}
module LambdaCube.Common.PrettyPrinter
  ( wrapIfSpaced
  , wrapIf
  , wrap

  , spaced
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

wrapIfSpaced :: Bool -> [Text] -> Text
wrapIfSpaced b = wrapIf b . spaced

wrapIf :: Bool -> Text -> Text
wrapIf True  = wrap
wrapIf False = id

wrap :: Text -> Text
wrap t = "(" <> t <> ")"

spaced :: [Text] -> Text
spaced = Text.intercalate " "
