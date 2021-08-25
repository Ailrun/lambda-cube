module LambdaCube.SystemF.Ast where

import           Data.Data                  (Data)
import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax (Lift)

data ExtLCTerm
  = ExtLCVar Text
  | ExtLCLam Text ExtLCType ExtLCTerm
  | ExtLCApp ExtLCTerm ExtLCTerm
  | ExtLCTLam Text ExtLCTerm
  | ExtLCTApp ExtLCTerm ExtLCType
  | ExtLCMVar String
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `ExtLCApp`
infixl 6 `ExtLCTApp`

data ExtLCType
  = ExtLCBase
  | ExtLCTVar Text
  | ExtLCArr ExtLCType ExtLCType
  | ExtLCUniv Text ExtLCType
  | ExtLCMTVar String
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `ExtLCArr`

data LCTerm
  = LCVar Int
  | LCLam LCType LCTerm
  | LCApp LCTerm LCTerm
  | LCTLam LCTerm
  | LCTApp LCTerm LCType
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `LCApp`
infixl 6 `LCTApp`

data LCType
  = LCBase
  | LCTVar Int
  | LCArr LCType LCType
  | LCUniv LCType
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `LCArr`

data LCValue
  = LCValLam LCType LCTerm
  | LCValTLam LCTerm
  deriving stock (Eq, Show, Data, Lift)

data LCNormalTerm
  = LCNormLam LCType LCNormalTerm
  | LCNormTLam LCNormalTerm
  | LCNormNeut LCNeutralTerm
  deriving stock (Eq, Show, Data, Lift)

data LCNeutralTerm
  = LCNeutVar Int
  | LCNeutApp LCNeutralTerm LCNormalTerm
  | LCNeutTApp LCNeutralTerm LCType
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `LCNeutApp`
infixl 6 `LCNeutTApp`
