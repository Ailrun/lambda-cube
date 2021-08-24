module LambdaCube.SystemFw.Parser
  ( pTopTerm
  , pTopType
  , pTopKind
  ) where

import           Data.Foldable            (Foldable (foldl'))
import           Data.Function            ((&))
import           Data.Functor             (($>))
import           Data.Maybe               (isJust)
import qualified Data.Text                as Text
import           LambdaCube.Common.Parser
import           LambdaCube.SystemFw.Ast
import           Text.Megaparsec

pTopTerm :: Parser ExtLCTerm
pTopTerm = topParser pTerm

pTerm :: Parser ExtLCTerm
pTerm = pTLam <|> pLam <|> pApp

pTLam :: Parser ExtLCTerm
pTLam =
  ExtLCTLam
  <$> (atsignBackslash *> identifier)
  <*> (colon *> pKind)
  <*> (dot *> pTerm)

pLam :: Parser ExtLCTerm
pLam =
  ExtLCLam
  <$> (backslash *> identifier)
  <*> (colon *> pType)
  <*> (dot *> pTerm)

pApp :: Parser ExtLCTerm
pApp = foldl' (&) <$> pATerm <*> many pAppArg

pAppArg :: Parser (ExtLCTerm -> ExtLCTerm)
pAppArg = do
  isType <- isJust <$> optional atsign
  if isType
    then flip ExtLCTApp <$> pAType
    else flip ExtLCApp <$> pATerm

pATerm :: Parser ExtLCTerm
pATerm = pVar <|> pMVar <|> parenthesized pTerm

pVar :: Parser ExtLCTerm
pVar = ExtLCVar <$> identifier

pMVar :: Parser ExtLCTerm
pMVar = ExtLCMVar <$> (dollarsign *> fmap Text.unpack identifier)

pTopType :: Parser ExtLCType
pTopType = topParser pType

pType :: Parser ExtLCType
pType = pTTLam <|> pUniv <|> pArr

pTTLam :: Parser ExtLCType
pTTLam =
  ExtLCTTLam
  <$> (backslash *> identifier)
  <*> (colon *> pKind)
  <*> (dot *> pType)

pUniv :: Parser ExtLCType
pUniv =
  ExtLCUniv
  <$> (exclamationMark *> identifier)
  <*> (colon *> pKind)
  <*> (comma *> pType)

pArr :: Parser ExtLCType
pArr = foldr1 ExtLCArr <$> sepBy1 pTTApp rightArrow

pTTApp :: Parser ExtLCType
pTTApp = foldl' ExtLCTTApp <$> pAType <*> many pAType

pAType :: Parser ExtLCType
pAType = pBase <|> pTVar <|> pMTVar <|> parenthesized pType

pBase :: Parser ExtLCType
pBase = sharp $> ExtLCBase

pTVar :: Parser ExtLCType
pTVar = ExtLCTVar <$> identifier

pMTVar :: Parser ExtLCType
pMTVar = ExtLCMTVar <$> (dollarsign *> fmap Text.unpack identifier)

pTopKind :: Parser ExtLCKind
pTopKind = topParser pKind

pKind :: Parser ExtLCKind
pKind = foldr1 ExtLCKArr <$> sepBy1 pAKind rightArrow

pAKind :: Parser ExtLCKind
pAKind = pStar <|> pMKVar <|> parenthesized pKind

pStar :: Parser ExtLCKind
pStar = asterisk $> ExtLCStar

pMKVar :: Parser ExtLCKind
pMKVar = ExtLCMKVar <$> (dollarsign *> fmap Text.unpack identifier)
