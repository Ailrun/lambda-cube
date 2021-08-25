module LambdaCube.SystemFw_.TH
  ( qTerm
  , qType
  , qKind
  ) where

import           Data.Data                   (Data)
import           Data.Generics               (extQ)
import           LambdaCube.Common.TH
import           LambdaCube.SystemFw_.Ast
import           LambdaCube.SystemFw_.Parser
import           Language.Haskell.TH.Lib     (ExpQ, varE)
import           Language.Haskell.TH.Quote   (QuasiQuoter (..))
import           Language.Haskell.TH.Syntax  (mkName)

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
      `extQ` quotedMKVar

    quotedMVar (ExtLCMVar x) = Just . varE $ mkName x
    quotedMVar _             = Nothing

    quotedMTVar (ExtLCMTVar x) = Just . varE $ mkName x
    quotedMTVar _              = Nothing

    quotedMKVar (ExtLCMKVar x) = Just . varE $ mkName x
    quotedMKVar _              = Nothing

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
      `extQ` quotedMKVar

    quotedMTVar (ExtLCMTVar x) = Just . varE $ mkName x
    quotedMTVar _              = Nothing

    quotedMKVar (ExtLCMKVar x) = Just . varE $ mkName x
    quotedMKVar _              = Nothing

qKind :: QuasiQuoter
qKind =
  QuasiQuoter
    { quoteExp = qExpKind
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = undefined
    }

qExpKind :: String -> ExpQ
qExpKind = qExpBase pTopKind converter
  where
    converter :: Data b => b -> Maybe ExpQ
    converter =
      converterBase
      `extQ` quotedMKVar

    quotedMKVar (ExtLCMKVar x) = Just . varE $ mkName x
    quotedMKVar _              = Nothing
