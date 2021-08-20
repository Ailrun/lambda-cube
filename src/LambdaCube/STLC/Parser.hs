module LambdaCube.STLC.Parser where

import           Data.Foldable            (Foldable (foldl'))
import           Data.Functor             (($>))
import           LambdaCube.Common.Parser
import           LambdaCube.STLC.Ast
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

pType :: Parser LCType
pType = foldr1 LCArr <$> sepBy1 pAType rightArrow

pAType :: Parser LCType
pAType = (sharp $> LCBase) <|> parenthesized pType
