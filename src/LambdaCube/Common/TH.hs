module LambdaCube.Common.TH
  ( qExpBase

  , converterBase
  ) where

import           Data.Data                  (Data)
import           Data.Generics              (extQ)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           LambdaCube.Common.Parser
import           Language.Haskell.TH.Lib    (ExpQ)
import           Language.Haskell.TH.Syntax (dataToExpQ, lift, loc_start,
                                             location)
import qualified Text.Megaparsec            as P

qExpBase :: Data a => Parser a -> (forall b. Data b => b -> Maybe ExpQ) -> String -> ExpQ
qExpBase p conv str = do
  l <- location
  case P.parse p ("<quote at " <> show (loc_start l) <> ">") (Text.pack str) of
    Right e  -> dataToExpQ conv e
    Left err -> fail $ P.errorBundlePretty err

converterBase :: Data b => b -> Maybe ExpQ
converterBase = const Nothing `extQ` (Just . lift :: Text -> Maybe ExpQ)
