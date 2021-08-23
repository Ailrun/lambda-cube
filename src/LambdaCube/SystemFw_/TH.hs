module LambdaCube.SystemFw_.TH
  ( lc
  ) where

import           Data.Data                   (Data)
import           Data.Generics               (extQ)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.Parser
import           Language.Haskell.TH.Lib     (ExpQ, varE)
import           Language.Haskell.TH.Quote   (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax  (Loc (loc_start), dataToExpQ, lift,
                                              location, mkName)
import qualified Text.Megaparsec             as P
import qualified Text.Megaparsec.Error       as PE

lc :: QuasiQuoter
lc =
  QuasiQuoter
    { quoteExp = expLc
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

expLc :: String -> ExpQ
expLc str = do
  l <- location
  case P.parse pTopLC ("<quote at " <> show (loc_start l) <> ">") (Text.pack str) of
    Right e  -> dataToExpQ converter e
    Left err -> fail $ PE.errorBundlePretty err
  where
    converter :: Data b => b -> Maybe ExpQ
    converter =
      const Nothing
      `extQ` quotedMVar
      `extQ` quotedMTVar
      `extQ` quotedMKVar
      `extQ` (Just . lift :: Text -> Maybe ExpQ)

    quotedMVar (ExtLCMVar x) = Just . varE $ mkName x
    quotedMVar _             = Nothing

    quotedMTVar (ExtLCMTVar x) = Just . varE $ mkName x
    quotedMTVar _              = Nothing

    quotedMKVar (ExtLCMKVar x) = Just . varE $ mkName x
    quotedMKVar _              = Nothing
