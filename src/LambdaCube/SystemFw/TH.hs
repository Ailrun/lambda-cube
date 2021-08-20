module LambdaCube.SystemFw.TH where

import           Control.Monad                  ((<=<))
import qualified Data.Text                      as Text
import           LambdaCube.SystemFw.Ast
import           LambdaCube.SystemFw.Elaborator
import           LambdaCube.SystemFw.Parser
import           Language.Haskell.TH.Quote      (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax     (Loc (loc_start), Q, lift,
                                                 location)
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Error          as PE

systemFw :: QuasiQuoter
systemFw =
  QuasiQuoter
    { quoteExp = lift <=< systemFwQuoteExp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

systemFwQuoteExp :: String -> Q ExtLCTerm
systemFwQuoteExp str = do
  l <- location
  case P.parse pTopLC ("<quote at " <> show (loc_start l) <> ">") (Text.pack str) of
    Right e  -> pure e
    Left err -> fail $ PE.errorBundlePretty err

elaboratedSystemFw :: QuasiQuoter
elaboratedSystemFw =
  QuasiQuoter
    { quoteExp = lift <=< elaboratedSystemFwQuoteExp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

elaboratedSystemFwQuoteExp :: String -> Q LCTerm
elaboratedSystemFwQuoteExp str = do
  e <- systemFwQuoteExp str
  case elaborate e of
    Right e' -> pure e'
    Left err -> fail err
