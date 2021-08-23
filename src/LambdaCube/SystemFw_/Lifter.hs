module LambdaCube.SystemFw_.Lifter where

import LambdaCube.SystemFw_.Ast

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b) = LCLam t b

liftLCNormal :: LCNormalTerm -> LCTerm
liftLCNormal (LCNormLam t b)  = LCLam t $ liftLCNormal b
liftLCNormal (LCNormNeut nt)  = liftLCNeutral nt

liftLCNeutral :: LCNeutralTerm -> LCTerm
liftLCNeutral (LCNeutVar n)    = LCVar n
liftLCNeutral (LCNeutApp f a)  = liftLCNeutral f `LCApp` liftLCNormal a
