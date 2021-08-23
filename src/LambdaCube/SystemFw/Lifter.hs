module LambdaCube.SystemFw.Lifter where

import           LambdaCube.SystemFw.Ast

liftLCValue :: LCValue -> LCTerm
liftLCValue (LCValLam t b)  = LCLam t b
liftLCValue (LCValTLam k b) = LCTLam k b

liftLCNormal :: LCNormalTerm -> LCTerm
liftLCNormal (LCNormLam t b)  = LCLam t $ liftLCNormal b
liftLCNormal (LCNormTLam k b) = LCTLam k $ liftLCNormal b
liftLCNormal (LCNormNeut nt)  = liftLCNeutral nt

liftLCNeutral :: LCNeutralTerm -> LCTerm
liftLCNeutral (LCNeutVar n)    = LCVar n
liftLCNeutral (LCNeutApp f a)  = liftLCNeutral f `LCApp` liftLCNormal a
liftLCNeutral (LCNeutTApp f t) = liftLCNeutral f `LCTApp` t
