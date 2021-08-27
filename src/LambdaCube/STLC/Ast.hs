module LambdaCube.STLC.Ast
  ( module LambdaCube.STLC.Ast
  , module LambdaCube.Common.Ast
  ) where

import           Data.Data                  (Data)
import           LambdaCube.Common.Ast
import           Language.Haskell.TH.Syntax (Lift)

data ExtLCModule
  = ExtLCModule [ExtLCModuleDecl]
  | ExtLCMMVar MetaIdentifier
  deriving stock (Eq, Show, Data, Lift)

data ExtLCModuleDecl
  = ExtLCModuleTermDecl Identifier ExtLCTerm
  | ExtLCModuleTypeDecl Identifier ExtLCType
  | ExtLCModuleSplice ExtLCModule
  deriving stock (Eq, Show, Data, Lift)

data ExtLCTerm
  = ExtLCVar Identifier
  | ExtLCLam Identifier ExtLCType ExtLCTerm
  | ExtLCApp ExtLCTerm ExtLCTerm
  | ExtLCMVar MetaIdentifier
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `ExtLCApp`

data ExtLCType
  = ExtLCBase
  | ExtLCTVar Identifier
  | ExtLCArr ExtLCType ExtLCType
  | ExtLCMTVar MetaIdentifier
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `ExtLCArr`

newtype LCModule = LCModule { getModuleDecls :: [LCModuleDecl] }
  deriving stock (Eq, Show, Data, Lift)

data LCModuleDecl
  = LCModuleTermDecl Identifier LCTerm
  | LCModuleTypeDecl Identifier LCType
  deriving stock (Eq, Show, Data, Lift)

data LCTerm
  = LCVar Int
  | LCGlobal Identifier
  | LCLam LCType LCTerm
  | LCApp LCTerm LCTerm
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `LCApp`

data LCType
  = LCBase
  | LCTGlobal Identifier
  | LCArr LCType LCType
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `LCArr`

data LCValue
  = LCValLam LCType LCTerm
  deriving stock (Eq, Show, Data, Lift)

data LCNormalTerm
  = LCNormLam LCType LCNormalTerm
  | LCNormNeut LCNeutralTerm
  deriving stock (Eq, Show, Data, Lift)

data LCNeutralTerm
  = LCNeutVar Int
  | LCNeutApp LCNeutralTerm LCNormalTerm
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `LCNeutApp`
