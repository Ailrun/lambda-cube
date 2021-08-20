module LambdaCube.SystemF.TH where

import           Control.Monad                 ((<=<))
import qualified Data.Text                     as Text
import           LambdaCube.SystemF.Ast
import           LambdaCube.SystemF.Elaborator
import           LambdaCube.SystemF.Parser
import           Language.Haskell.TH.Quote     (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax    (Loc (loc_start), Q, lift,
                                                location)
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Error         as PE

systemF :: QuasiQuoter
systemF =
  QuasiQuoter
    { quoteExp = lift <=< systemFQuoteExp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

systemFQuoteExp :: String -> Q ExtLCTerm
systemFQuoteExp str = do
  l <- location
  case P.parse pTopLC ("<quote at " <> show (loc_start l) <> ">") (Text.pack str) of
    Right e  -> pure e
    Left err -> fail $ PE.errorBundlePretty err

elaboratedSystemF :: QuasiQuoter
elaboratedSystemF =
  QuasiQuoter
    { quoteExp = lift <=< elaboratedSystemFQuoteExp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

elaboratedSystemFQuoteExp :: String -> Q LCTerm
elaboratedSystemFQuoteExp str = do
  e <- systemFQuoteExp str
  case elaborate e of
    Right e' -> pure e'
    Left err -> fail err
