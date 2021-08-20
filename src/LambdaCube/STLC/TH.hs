module LambdaCube.STLC.TH where

import           Control.Monad              ((<=<))
import qualified Data.Text                  as Text
import           LambdaCube.STLC.Ast
import           LambdaCube.STLC.Elaborator
import           LambdaCube.STLC.Parser
import           Language.Haskell.TH.Quote  (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax (Loc (loc_start), Q, lift, location)
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Error      as PE

stlc :: QuasiQuoter
stlc =
  QuasiQuoter
    { quoteExp = lift <=< stlcQuoteExp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

stlcQuoteExp :: String -> Q ExtLCTerm
stlcQuoteExp str = do
  l <- location
  case P.parse pTopLC ("<quote at " <> show (loc_start l) <> ">") (Text.pack str) of
    Right e  -> pure e
    Left err -> fail $ PE.errorBundlePretty err

elaboratedStlc :: QuasiQuoter
elaboratedStlc =
  QuasiQuoter
    { quoteExp = lift <=< elaboratedStlcQuoteExp
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

elaboratedStlcQuoteExp :: String -> Q LCTerm
elaboratedStlcQuoteExp str = do
  e <- stlcQuoteExp str
  case elaborate e of
    Right e' -> pure e'
    Left err -> fail err
