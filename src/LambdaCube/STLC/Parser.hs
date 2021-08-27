module LambdaCube.STLC.Parser
  ( pTopModule
  , pTopTerm
  , pTopType
  ) where

import           Data.Foldable            (Foldable (foldl'))
import           Data.Functor             (($>))
import           LambdaCube.Common.Parser
import           LambdaCube.STLC.Ast
import           Text.Megaparsec

pTopModule :: Parser ExtLCModule
pTopModule = topParser pModule

pModule :: Parser ExtLCModule
pModule = ExtLCModule <$> sepEndBy pModuleDecl semicolon

pModuleDecl :: Parser ExtLCModuleDecl
pModuleDecl = pModuleSplice <|> pModuleTypeDecl <|> pModuleTermDecl

pModuleSplice :: Parser ExtLCModuleDecl
pModuleSplice = ExtLCModuleSplice . ExtLCMMVar <$> metaIdentifier

pModuleTermDecl :: Parser ExtLCModuleDecl
pModuleTermDecl =
  ExtLCModuleTermDecl
  <$> identifier
  <*> (equalsign *> pTerm)

pModuleTypeDecl :: Parser ExtLCModuleDecl
pModuleTypeDecl =
  ExtLCModuleTypeDecl
  <$> (tripleColon *> identifier)
  <*> (equalsign *> pType)

pTopTerm :: Parser ExtLCTerm
pTopTerm = topParser pTerm

pTerm :: Parser ExtLCTerm
pTerm = pLam <|> pApp

pLam :: Parser ExtLCTerm
pLam =
  ExtLCLam
  <$> (backslash *> identifier)
  <*> (colon *> pType)
  <*> (dot *> pTerm)

pApp :: Parser ExtLCTerm
pApp = foldl' ExtLCApp <$> pATerm <*> many pATerm

pATerm :: Parser ExtLCTerm
pATerm = pVar <|> pMVar <|> parenthesized pTerm

pVar :: Parser ExtLCTerm
pVar = ExtLCVar <$> identifier

pMVar :: Parser ExtLCTerm
pMVar = ExtLCMVar <$> metaIdentifier

pTopType :: Parser ExtLCType
pTopType = topParser pType

pType :: Parser ExtLCType
pType = foldr1 ExtLCArr <$> sepBy1 pAType rightArrow

pAType :: Parser ExtLCType
pAType = pBase <|> pMTVar <|> parenthesized pType

pBase :: Parser ExtLCType
pBase = sharp $> ExtLCBase

pMTVar :: Parser ExtLCType
pMTVar = ExtLCMTVar <$> metaIdentifier
