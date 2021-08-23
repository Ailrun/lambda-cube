module LambdaCube.STLC.Ast where

import           Data.Data                  (Data)
import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax (Lift)

data ExtLCType
  = ExtLCBase
  | ExtLCArr ExtLCType ExtLCType
  | ExtLCMTVar String
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `ExtLCArr`

data ExtLCTerm
  = ExtLCVar Text
  | ExtLCLam Text ExtLCType ExtLCTerm
  | ExtLCApp ExtLCTerm ExtLCTerm
  | ExtLCMVar String
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `ExtLCApp`

data LCType
  = LCBase
  | LCArr LCType LCType
  deriving stock (Eq, Show, Data, Lift)
infixr 5 `LCArr`

data LCTerm
  = LCVar Int
  | LCLam LCType LCTerm
  | LCApp LCTerm LCTerm
  deriving stock (Eq, Show, Data, Lift)
infixl 6 `LCApp`

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
