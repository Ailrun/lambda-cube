module LambdaCube.STLC.Ast where

import           Data.Text                  (Text)
import           Language.Haskell.TH.Syntax (Lift)

data ExtLCTerm
  = ExtLCVar Text
  | ExtLCLam Text LCType ExtLCTerm
  | ExtLCApp ExtLCTerm ExtLCTerm
  deriving stock (Eq, Show, Lift)
infixl 6 `ExtLCApp`

data LCType
  = LCBase
  | LCArr LCType LCType
  deriving stock (Eq, Show, Lift)
infixr 5 `LCArr`

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
