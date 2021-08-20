module LambdaCube.SystemFw_.TH where

import           Control.Monad                   ((<=<))
import qualified Data.Text                       as Text
import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.Elaborator
import           LambdaCube.SystemFw_.Parser
import           Language.Haskell.TH.Quote       (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax      (Loc (loc_start), Q, lift,
                                                  location)
import qualified Text.Megaparsec                 as P
import qualified Text.Megaparsec.Error           as PE

systemFw_ :: QuasiQuoter
systemFw_ =
  QuasiQuoter
    { quoteExp = lift <=< systemFw_QuoteExp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

systemFw_QuoteExp :: String -> Q ExtLCTerm
systemFw_QuoteExp str = do
  l <- location
  case P.parse pTopLC ("<quote at " <> show (loc_start l) <> ">") (Text.pack str) of
    Right e  -> pure e
    Left err -> fail $ PE.errorBundlePretty err

elaboratedSystemFw_ :: QuasiQuoter
elaboratedSystemFw_ =
  QuasiQuoter
    { quoteExp = lift <=< elaboratedSystemFw_QuoteExp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

elaboratedSystemFw_QuoteExp :: String -> Q LCTerm
elaboratedSystemFw_QuoteExp str = do
  e <- systemFw_QuoteExp str
  case elaborate e of
    Right e' -> pure e'
    Left err -> fail err
