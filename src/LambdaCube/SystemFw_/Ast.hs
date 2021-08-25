module LambdaCube.SystemFw_.Ast where

import           Data.Data                  (Data)
import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax (Lift)

data ExtLCTerm
  = ExtLCVar Text
  | ExtLCLam Text ExtLCType ExtLCTerm
  | ExtLCApp ExtLCTerm ExtLCTerm
  | ExtLCMVar String
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `ExtLCApp`

data ExtLCType
  = ExtLCBase
  | ExtLCTVar Text
  | ExtLCArr ExtLCType ExtLCType
  | ExtLCTTLam Text ExtLCKind ExtLCType
  | ExtLCTTApp ExtLCType ExtLCType
  | ExtLCMTVar String
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `ExtLCArr`
infixl 6 `ExtLCTTApp`

data ExtLCKind
  = ExtLCStar
  | ExtLCKArr ExtLCKind ExtLCKind
  | ExtLCMKVar String
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `ExtLCKArr`

data LCTerm
  = LCVar Int
  | LCLam LCType LCTerm
  | LCApp LCTerm LCTerm
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `LCApp`

data LCType
  = LCBase
  | LCTVar Int
  | LCArr LCType LCType
  | LCTTLam LCKind LCType
  | LCTTApp LCType LCType
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `LCArr`
infixl 6 `LCTTApp`

data LCKind
  = LCStar
  | LCKArr LCKind LCKind
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `LCKArr`

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
