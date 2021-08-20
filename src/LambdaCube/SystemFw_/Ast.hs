module LambdaCube.SystemFw_.Ast where

import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax (Lift)

data ExtLCType
  = ExtLCBase
  | ExtLCTVar Text
  | ExtLCArr ExtLCType ExtLCType
  | ExtLCTTLam Text LCKind ExtLCType
  | ExtLCTTApp ExtLCType ExtLCType
  deriving stock (Eq, Show, Lift)
infixr 5 `ExtLCArr`
infixl 6 `ExtLCTTApp`

data ExtLCTerm
  = ExtLCVar Text
  | ExtLCLam Text ExtLCType ExtLCTerm
  | ExtLCApp ExtLCTerm ExtLCTerm
  deriving stock (Eq, Show, Lift)
infixl 6 `ExtLCApp`

data LCKind
  = LCStar
  | LCKArr LCKind LCKind
  deriving stock (Eq, Show, Lift)
infixr 5 `LCKArr`

data LCType
  = LCBase
  | LCTVar Int
  | LCArr LCType LCType
  | LCTTLam LCKind LCType
  | LCTTApp LCType LCType
  deriving stock (Eq, Show, Lift)
infixr 5 `LCArr`
infixl 6 `LCTTApp`

data LCTerm
  = LCVar Int
  | LCLam LCType LCTerm
  | LCApp LCTerm LCTerm
  deriving stock (Eq, Show, Lift)
infixl 6 `LCApp`

data LCValue
  = LCValLam LCType LCTerm
  deriving stock (Eq, Show, Lift)

data LCNormalTerm
  = LCNormLam LCType LCNormalTerm
  | LCNormNeut LCNeutralTerm
  deriving stock (Eq, Show, Lift)

data LCNeutralTerm
  = LCNeutVar Int
  | LCNeutApp LCNeutralTerm LCNormalTerm
  deriving stock (Eq, Show, Lift)
infixl 6 `LCNeutApp`
