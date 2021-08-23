module LambdaCube.STLC.Parser where

import           Data.Foldable            (Foldable (foldl'))
import           Data.Functor             (($>))
import qualified Data.Text                as Text
import           LambdaCube.Common.Parser
import           LambdaCube.STLC.Ast
import           Text.Megaparsec

pTopLC :: Parser ExtLCTerm
pTopLC = topParser pLC

pLC :: Parser ExtLCTerm
pLC = pLam<|> pApp

pLam :: Parser ExtLCTerm
pLam =
  ExtLCLam
  <$> (backslash *> identifier)
  <*> (colon *> pType)
  <*> (dot *> pLC)

pApp :: Parser ExtLCTerm
pApp = foldl' ExtLCApp <$> pATerm <*> many pATerm

pATerm :: Parser ExtLCTerm
pATerm = pVar <|> pMVar <|> parenthesized pLC

pVar :: Parser ExtLCTerm
pVar = ExtLCVar <$> identifier

pMVar :: Parser ExtLCTerm
pMVar = ExtLCMVar <$> (dollarsign *> fmap Text.unpack identifier)

pType :: Parser ExtLCType
pType = foldr1 ExtLCArr <$> sepBy1 pAType rightArrow

pAType :: Parser ExtLCType
pAType = pBase <|> pMTVar <|> parenthesized pType

pBase :: Parser ExtLCType
pBase = sharp $> ExtLCBase

pMTVar :: Parser ExtLCType
pMTVar = ExtLCMTVar <$> (dollarsign *> fmap Text.unpack identifier)
