module LambdaCube.SystemFw.Parser where

import           Data.Foldable            (Foldable (foldl'))
import           Data.Function            ((&))
import           Data.Functor             (($>))
import           Data.Maybe               (isJust)
import           LambdaCube.Common.Parser
import           LambdaCube.SystemFw.Ast
import           Text.Megaparsec

pTopLC :: Parser ExtLCTerm
pTopLC = topParser pLC

pLC :: Parser ExtLCTerm
pLC = pTLam <|> pLam <|> pApp

pTLam :: Parser ExtLCTerm
pTLam =
  ExtLCTLam
  <$> (atsignBackslash *> identifier)
  <*> (colon *> pKind)
  <*> (dot *> pLC)

pLam :: Parser ExtLCTerm
pLam =
  ExtLCLam
  <$> (backslash *> identifier)
  <*> (colon *> pType)
  <*> (dot *> pLC)

pApp :: Parser ExtLCTerm
pApp = foldl' (&) <$> pATerm <*> many pAppArg

pAppArg :: Parser (ExtLCTerm -> ExtLCTerm)
pAppArg = do
  isType <- isJust <$> optional atsign
  if isType
    then flip ExtLCTApp <$> pAType
    else flip ExtLCApp <$> pATerm

pATerm :: Parser ExtLCTerm
pATerm = (ExtLCVar <$> identifier) <|> parenthesized pLC

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
pAType = (sharp $> ExtLCBase) <|> (ExtLCTVar <$> identifier) <|> parenthesized pType

pKind :: Parser LCKind
pKind = foldr1 LCKArr <$> sepBy1 pAKind rightArrow

pAKind :: Parser LCKind
pAKind = (asterisk $> LCStar) <|> parenthesized pKind
