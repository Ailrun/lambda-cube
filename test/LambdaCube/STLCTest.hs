{-# LANGUAGE QuasiQuotes #-}
module LambdaCube.STLCTest
  ( tests
  ) where

import qualified Control.Exception
import           Control.Monad              (forM_, void)
import qualified Data.Text                  as Text
import           LambdaCube.STLC
import           LambdaCube.STLCTestExample
import           LambdaCube.TestUtil
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec           (testSpec)

newtype TestLCType = TestLCType LCType
  deriving newtype (Eq)

instance Show TestLCType where
  show (TestLCType ty) = Text.unpack $ prettyUnnamedType ty

newtype TestLCTerm = TestLCTerm LCTerm
  deriving newtype (Eq)

instance Show TestLCTerm where
  show (TestLCTerm tm) = Text.unpack $ prettyUnnamedTerm tm

tests :: IO TestTree
tests = testSpec "STLC tests" spec

spec :: Spec
spec = do
  describe "STLC infer" inferSpec
  describe "STLC evaluate" evaluateSpec
  describe "STLC normalize" normalizeSpec

inferSpec :: SpecWith ()
inferSpec = forM_ inferSpecCases $ \(mkTitle, extTm, extTy) -> do
  let
    tm = elaborate extTm
    ty = elaborateType extTy
  it (mkTitle tm ty) $ do
    TestLCType (infer tm) `shouldBe` TestLCType ty

inferSpecCases :: [(LCTerm -> LCType -> String, ExtLCTerm, ExtLCType)]
inferSpecCases =
  [ ( const2 "infer lcBaseId should be lcBaseArr"
    , lcBaseId
    , lcBaseArr
    )
  , ( makeDefaultInferTitle
    , [qTerm| \x : # . $lcBaseArrId $lcBaseId x |]
    , lcBaseArr
    )
  , ( makeDefaultInferTitle
    , [qTerm| \x : $lcBaseArr . \y : $lcBaseArr . \z : # . x (y z) |]
    , [qType| $lcBaseArr -> $lcBaseArr -> $lcBaseArr |]
    )
  , ( makeDefaultInferTitle
    , [qTerm| \abc : # . (\fdd : $lcBaseArr . fdd abc) |]
    , [qType| # -> $lcBaseArr -> # |]
    )
  , ( const2 "infer pcn3 should be pcnTy"
    , lcPCN3
    , lcPCNTy
    )
  , ( const2 "infer lcPCNAdd should be (lcPCNTy -> lcPCNTy -> lcPCNTy)"
    , lcPCNAdd
    , [qType| $lcPCNTy -> $lcPCNTy -> $lcPCNTy |]
    )
  , ( const2 "infer (lcPCNAdd lcPCN1 lcPCN2) should be lcPCNTy"
    , [qTerm| $lcPCNAdd $lcPCN1 $lcPCN2 |]
    , [qType| $lcPCNTy |]
    )
  , ( const2 "infer lcPCNMul should be (lcPCNTy -> lcPCNTy -> lcPCNTy)"
    , lcPCNMul
    , [qType| $lcPCNTy -> $lcPCNTy -> $lcPCNTy |]
    )
  , ( const2 "infer (lcPCNMul lcPCN0) should be (lcPCNTy -> lcPCNTy)"
    , [qTerm| $lcPCNMul $lcPCN0 |]
    , [qType| $lcPCNTy -> $lcPCNTy |]
    )
  ]

evaluateSpec :: SpecWith ()
evaluateSpec = forM_ evaluateSpecCases $ \(mkTitle, extTm, extResTm) -> do
  let
    tm = elaborate extTm
    resTm = elaborate extResTm
  it (mkTitle tm resTm) $ do
    void . Control.Exception.evaluate $ infer tm
    void . Control.Exception.evaluate $ infer resTm
    TestLCTerm (liftLCValue (evaluate tm)) `shouldBe` TestLCTerm (liftLCValue (evaluate resTm))

evaluateSpecCases :: [(LCTerm -> LCTerm -> String, ExtLCTerm, ExtLCTerm)]
evaluateSpecCases =
  [ ( makeDefaultEvaluateTitle
    , [qTerm| (\abc : $lcBaseArr . (\fdd : $lcBaseArr -> $lcBaseArr . fdd abc)) $lcBaseId |]
    , [qTerm| \fdd : $lcBaseArr -> $lcBaseArr . fdd $lcBaseId |]
    )
  , ( const2 "evaluate (lcBaseArrId lcBaseId) should be lcBaseId"
    , [qTerm| $lcBaseArrId $lcBaseId |]
    , lcBaseId
    )
  , ( const2 "evaluate (lcBaseArrId (lcBaseArrId lcBaseId)) should be lcBaseId"
    , [qTerm| $lcBaseArrId ($lcBaseArrId $lcBaseId) |]
    , lcBaseId
    )
  , ( const2 "evaluate (lcPCNAdd lcPCN1 lcPCN2) should be lcPCN3 with unnormalized parts"
    , [qTerm| $lcPCNAdd $lcPCN1 $lcPCN2 |]
    , [qTerm| \s : $lcBaseArr . \z : # . $lcPCN1 s ($lcPCN2 s z) |]
    )
  ]

normalizeSpec :: SpecWith ()
normalizeSpec = forM_ normalizeSpecCases $ \(mkTitle, extTm, extResTm) -> do
  let
    tm = elaborate extTm
    resTm = elaborate extResTm
  it (mkTitle tm resTm) $ do
    void . Control.Exception.evaluate $ infer tm
    void . Control.Exception.evaluate $ infer resTm
    TestLCTerm (liftLCNormal (normalize tm)) `shouldBe` TestLCTerm (liftLCNormal (normalize resTm))

normalizeSpecCases :: [(LCTerm -> LCTerm -> String, ExtLCTerm, ExtLCTerm)]
normalizeSpecCases =
  [ ( makeDefaultNormalizeTitle
    , [qTerm| (\abc : $lcBaseArr . (\fdd : $lcBaseArr -> $lcBaseArr . fdd abc)) $lcBaseId |]
    , [qTerm| \fdd : $lcBaseArr -> $lcBaseArr . fdd $lcBaseId |]
    )
  , ( const2 "normalize (lcBaseArrId lcBaseId) should be lcBaseId"
    , [qTerm| $lcBaseArrId $lcBaseId |]
    , lcBaseId
    )
  , ( const2 "normalize (lcBaseArrId (lcBaseArrId lcBaseId)) should be lcBaseId"
    , [qTerm| $lcBaseArrId ($lcBaseArrId $lcBaseId) |]
    , lcBaseId
    )
  , ( const2 "normalize (lcPCNAdd lcPCN1 lcPCN2) should be lcPCN3"
    , [qTerm| $lcPCNAdd $lcPCN0 $lcPCN1 |]
    , lcPCN1
    )
  , ( const2 "normalize (lcPCN4 lcBaseId) should be lcBaseId"
    , [qTerm| $lcPCN4 $lcBaseId |]
    , lcBaseId
    )
  ]

makeDefaultInferTitle :: LCTerm -> LCType -> String
makeDefaultInferTitle tm ty =
  makeDefaultTitle
  "infer"
  (prettyShowUnnamedTerm tm)
  (prettyShowUnnamedType ty)

makeDefaultEvaluateTitle :: LCTerm -> LCTerm -> String
makeDefaultEvaluateTitle tm resTm =
  makeDefaultTitle
  "evaluate"
  (prettyShowUnnamedTerm tm)
  (prettyShowUnnamedTerm resTm)

makeDefaultNormalizeTitle :: LCTerm -> LCTerm -> String
makeDefaultNormalizeTitle tm resTm =
  makeDefaultTitle
  "normalize"
  (prettyShowUnnamedTerm tm)
  (prettyShowUnnamedTerm resTm)
