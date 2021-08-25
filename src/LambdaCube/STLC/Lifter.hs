module LambdaCube.STLC.Lifter
  ( liftLCValue
  , liftLCNormal
  ) where

import           LambdaCube.STLC.Ast

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b) = LCLam t b

liftLCNormal :: LCNormalTerm -> LCTerm
liftLCNormal (LCNormLam t b) = LCLam t $ liftLCNormal b
liftLCNormal (LCNormNeut nt) = liftLCNeutral nt

liftLCNeutral :: LCNeutralTerm -> LCTerm
liftLCNeutral (LCNeutVar x) = LCVar x
liftLCNeutral (LCNeutApp f a) = liftLCNeutral f `LCApp` liftLCNormal a
