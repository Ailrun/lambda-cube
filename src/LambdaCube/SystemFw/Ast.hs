module LambdaCube.SystemFw.Ast where

import           Data.Data                  (Data)
import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax (Lift)

data ExtLCKind
  = ExtLCStar
  | ExtLCKArr ExtLCKind ExtLCKind
  | ExtLCMKVar String
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `ExtLCKArr`

data ExtLCType
  = ExtLCBase
  | ExtLCTVar Text
  | ExtLCArr ExtLCType ExtLCType
  | ExtLCUniv Text ExtLCKind ExtLCType
  | ExtLCTTLam Text ExtLCKind ExtLCType
  | ExtLCTTApp ExtLCType ExtLCType
  | ExtLCMTVar String
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `ExtLCArr`
infixl 6 `ExtLCTTApp`

data ExtLCTerm
  = ExtLCVar Text
  | ExtLCLam Text ExtLCType ExtLCTerm
  | ExtLCApp ExtLCTerm ExtLCTerm
  | ExtLCTLam Text ExtLCKind ExtLCTerm
  | ExtLCTApp ExtLCTerm ExtLCType
  | ExtLCMVar String
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `ExtLCApp`
infixl 6 `ExtLCTApp`

data LCKind
  = LCStar
  | LCKArr LCKind LCKind
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `LCKArr`

data LCType
  = LCBase
  | LCTVar Int
  | LCArr LCType LCType
  | LCUniv LCKind LCType
  | LCTTLam LCKind LCType
  | LCTTApp LCType LCType
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `LCArr`
infixl 6 `LCTTApp`

data LCTerm
  = LCVar Int
  | LCLam LCType LCTerm
  | LCApp LCTerm LCTerm
  | LCTLam LCKind LCTerm
  | LCTApp LCTerm LCType
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `LCApp`
infixl 6 `LCTApp`

data LCValue
  = LCValLam LCType LCTerm
  | LCValTLam LCKind LCTerm
  deriving stock (Eq, Show, Data, Lift)

data LCNormalTerm
  = LCNormLam LCType LCNormalTerm
  | LCNormTLam LCKind LCNormalTerm
  | LCNormNeut LCNeutralTerm
  deriving stock (Eq, Show, Data, Lift)

data LCNeutralTerm
  = LCNeutVar Int
  | LCNeutApp LCNeutralTerm LCNormalTerm
  | LCNeutTApp LCNeutralTerm LCType
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `LCNeutApp`
infixl 6 `LCNeutTApp`
