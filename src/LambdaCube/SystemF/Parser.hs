module LambdaCube.SystemF.Parser where

import           Data.Foldable            (Foldable (foldl'))
import           Data.Function            ((&))
import           Data.Functor             (($>))
import           Data.Maybe               (isJust)
import           LambdaCube.Common.Parser
import           LambdaCube.SystemF.Ast
import           Text.Megaparsec

pTopLC :: Parser ExtLCTerm
pTopLC = topParser pLC

pLC :: Parser ExtLCTerm
pLC = pTLam <|> pLam <|> pApp

pTLam :: Parser ExtLCTerm
pTLam =
  ExtLCTLam
  <$> (atsignBackslash *> identifier <* colon <* asterisk)
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
pType = pUniv <|> pArr

pUniv :: Parser ExtLCType
pUniv =
  ExtLCUniv
  <$> (exclamationMark *> identifier <* colon <* asterisk)
  <*> (comma *> pType)

pArr :: Parser ExtLCType
pArr = foldr1 ExtLCArr <$> sepBy1 pAType rightArrow

pAType :: Parser ExtLCType
pAType = (sharp $> ExtLCBase) <|> (ExtLCTVar <$> identifier) <|> parenthesized pType
