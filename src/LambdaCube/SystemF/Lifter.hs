module LambdaCube.SystemF.Lifter
  ( liftLCValue
  , liftLCNormal
  ) where

import           LambdaCube.SystemF.Ast

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b) = LCLam t b
liftLCValue (LCValTLam b)  = LCTLam b

liftLCNormal :: LCNormalTerm -> LCTerm
liftLCNormal (LCNormLam t b) = LCLam t $ liftLCNormal b
liftLCNormal (LCNormTLam b)  = LCTLam $ liftLCNormal b
liftLCNormal (LCNormNeut nt) = liftLCNeutral nt

liftLCNeutral :: LCNeutralTerm -> LCTerm
liftLCNeutral (LCNeutVar x)    = LCVar x
liftLCNeutral (LCNeutApp f a)  = liftLCNeutral f `LCApp` liftLCNormal a
liftLCNeutral (LCNeutTApp f t) = liftLCNeutral f `LCTApp` t
