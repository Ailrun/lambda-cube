module LambdaCube.SystemFw_.Parser where

import           Data.Foldable            (Foldable (foldl'))
import           Data.Functor             (($>))
import qualified Data.Text                as Text
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
pATerm = pVar <|> pMVar <|> parenthesized pLC

pVar :: Parser ExtLCTerm
pVar = ExtLCVar <$> identifier

pMVar :: Parser ExtLCTerm
pMVar = ExtLCMVar <$> (dollarsign *> fmap Text.unpack identifier)

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
pAType = pBase <|> pTVar <|> pMTVar <|> parenthesized pType

pBase :: Parser ExtLCType
pBase = sharp $> ExtLCBase

pTVar :: Parser ExtLCType
pTVar = ExtLCTVar <$> identifier

pMTVar :: Parser ExtLCType
pMTVar = ExtLCMTVar <$> (dollarsign *> fmap Text.unpack identifier)

pKind :: Parser ExtLCKind
pKind = foldr1 ExtLCKArr <$> sepBy1 pAKind rightArrow

pAKind :: Parser ExtLCKind
pAKind = pStar <|> pMKVar <|> parenthesized pKind

pStar :: Parser ExtLCKind
pStar = asterisk $> ExtLCStar

pMKVar :: Parser ExtLCKind
pMKVar = ExtLCMKVar <$> (dollarsign *> fmap Text.unpack identifier)
