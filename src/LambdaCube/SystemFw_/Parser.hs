module LambdaCube.SystemFw_.Parser where

import           Data.Foldable            (Foldable (foldl'))
import           Data.Functor             (($>))
import           LambdaCube.Common.Parser
import           LambdaCube.SystemFw_.Ast
import           Text.Megaparsec

pTopLC :: Parser ExtLCTerm
pTopLC = topParser pLC

pLC :: Parser ExtLCTerm
pLC = pLam <|> pApp

pLam :: Parser ExtLCTerm
pLam =
  ExtLCLam
  <$> (backslash *> identifier)
  <*> (colon *> pType)
  <*> (dot *> pLC)

pApp :: Parser ExtLCTerm
pApp = foldl' ExtLCApp <$> pATerm <*> many pATerm

pATerm :: Parser ExtLCTerm
pATerm = (ExtLCVar <$> identifier) <|> parenthesized pLC

pType :: Parser ExtLCType
pType = pTTLam <|> pArr

pTTLam :: Parser ExtLCType
pTTLam =
  ExtLCTTLam
  <$> (backslash *> identifier)
  <*> (colon *> pKind)
  <*> (dot *> pType)

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
