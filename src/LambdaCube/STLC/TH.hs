module LambdaCube.STLC.TH
  ( qTerm
  , qType
  ) where

import           Data.Data                  (Data)
import           Data.Generics              (extQ)
import           LambdaCube.Common.TH
import           LambdaCube.STLC.Ast
import           LambdaCube.STLC.Parser
import           Language.Haskell.TH.Lib    (ExpQ, varE)
import           Language.Haskell.TH.Quote  (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax (mkName)

qTerm :: QuasiQuoter
qTerm =
  QuasiQuoter
    { quoteExp = qExpTerm
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

qExpTerm :: String -> ExpQ
qExpTerm = qExpBase pTopTerm converter
  where
    converter :: Data b => b -> Maybe ExpQ
    converter =
      converterBase
      `extQ` quotedMVar
      `extQ` quotedMTVar

    quotedMVar (ExtLCMVar x) = Just . varE $ mkName x
    quotedMVar _             = Nothing

    quotedMTVar (ExtLCMTVar x) = Just . varE $ mkName x
    quotedMTVar _              = Nothing

qType :: QuasiQuoter
qType =
  QuasiQuoter
    { quoteExp = qExpType
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

qExpType :: String -> ExpQ
qExpType = qExpBase pTopType converter
  where
    converter :: Data b => b -> Maybe ExpQ
    converter =
      converterBase
      `extQ` quotedMTVar

    quotedMTVar (ExtLCMTVar x) = Just . varE $ mkName x
    quotedMTVar _              = Nothing
