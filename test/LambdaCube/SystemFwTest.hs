{-# LANGUAGE QuasiQuotes #-}
module LambdaCube.SystemFwTest where

import qualified Control.Exception
import           Control.Monad                  (forM_, void)
import qualified Data.Text                      as Text
import           LambdaCube.SystemFw
import           LambdaCube.SystemFwTestExample
import           LambdaCube.TestUtil
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec               (testSpec)

newtype TestLCType = TestLCType LCType
  deriving newtype (Eq)

instance Show TestLCType where
  show (TestLCType ty) = Text.unpack $ prettyUnnamedType ty

newtype TestLCTerm = TestLCTerm LCTerm
  deriving newtype (Eq)

instance Show TestLCTerm where
  show (TestLCTerm tm) = Text.unpack $ prettyUnnamedTerm tm

tests :: IO TestTree
tests = testSpec "SystemFw tests" spec

spec :: Spec
spec = do
  describe "SystemFw infer" inferSpec
  describe "SystemFw evaluate" evaluateSpec
  describe "SystemFw normalize" normalizeSpec

inferSpec :: SpecWith ()
inferSpec = forM_ inferSpecCases $ \(mkTitle, extTm, extTy) -> do
  let
    tm = elaborate extTm
    ty = elaborateType [] extTy
  it (mkTitle tm ty) $ do
    TestLCType (infer tm) `shouldBe` TestLCType (reduceType ty)

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
  , ( const2 "infer lcId should be (! a : * , a -> a)"
    , lcId
    , [qType| ! a : * , a -> a |]
    )
  , ( const2 "infer pCN3 should be pCNTy"
    , lcCN3
    , lcCNTy
    )
  , ( const2 "infer lcCNSucc should be (lcCNTy -> lcCNTy)"
    , [qTerm| $lcCNSucc |]
    , [qType| $lcCNTy -> $lcCNTy |]
    )
  , ( const2 "infer lcCNAdd should be (lcCNTy -> lcCNTy -> lcCNTy)"
    , lcCNAdd
    , [qType| $lcCNTy -> $lcCNTy -> $lcCNTy |]
    )
  , ( const2 "infer (lcCNAdd lcCN1 lcCN2) should be lcCNTy"
    , [qTerm| $lcCNAdd $lcCN1 $lcCN2 |]
    , [qType| $lcCNTy |]
    )
  , ( const2 "infer lcCNMul should be (lcCNTy -> lcCNTy -> lcCNTy)"
    , lcCNMul
    , [qType| $lcCNTy -> $lcCNTy -> $lcCNTy |]
    )
  , ( const2 "infer (lcCNMul lcCN0) should be (lcCNTy -> lcCNTy)"
    , [qTerm| $lcCNMul $lcCN0 |]
    , [qType| $lcCNTy -> $lcCNTy |]
    )
  , ( const2 "infer lcCNPred should be (lcCNTy -> lcCNTy)"
    , [qTerm| $lcCNPred |]
    , [qType| $lcCNTy -> $lcCNTy |]
    )
  , ( const2 "infer lcCNPredDef should be (lcCNTy -> lcCNTy -> lcCNTy)"
    , [qTerm| $lcCNPredDef |]
    , [qType| $lcCNTy -> $lcCNTy -> $lcCNTy |]
    )
  , ( const2 "infer lcCNMinus should be (lcCNTy -> lcCNTy -> lcCNTy)"
    , [qTerm| $lcCNMinus |]
    , [qType| $lcCNTy -> $lcCNTy -> $lcCNTy |]
    )
  , ( const2 "infer lcCNIs0 should be (lcCNTy -> lcCBTy)"
    , [qTerm| $lcCNIs0 |]
    , [qType| $lcCNTy -> $lcCBTy |]
    )
  , ( const2 "infer lcCLNil should be (!e : * , lcCLTy e)"
    , [qTerm| $lcCLNil |]
    , [qType| !e : * , $lcCLTy e |]
    )
  , ( const2 "infer lcCLCons should be (!e : * , e -> lcCLTy e -> lcCLTy e)"
    , [qTerm| $lcCLCons |]
    , [qType| !e : * , e -> $lcCLTy e -> $lcCLTy e |]
    )
  , ( const2 "infer lcCL_CN_0_1_2 should be (lcCLTy lcCNTy)"
    , [qTerm| $lcCL_CN_0_1_2 |]
    , [qType| $lcCLTy $lcCNTy |]
    )
  , ( const2 "infer lcCLHeadDef should be (!e : * , e -> lcCLTy e -> e)"
    , [qTerm| $lcCLHeadDef |]
    , [qType| !e : * , e -> $lcCLTy e -> e |]
    )
  , ( const2 "infer lcCLTail should be (!e : * , lcCLTy e -> lcCLTy e)"
    , [qTerm| $lcCLTail |]
    , [qType| !e : * , $lcCLTy e -> $lcCLTy e |]
    )
  , ( const2 "infer lcCLTailDef should be (!e : * , lcCLTy e -> lcCLTy e -> lcCLTy e)"
    , [qTerm| $lcCLTailDef |]
    , [qType| !e : * , $lcCLTy e -> $lcCLTy e -> $lcCLTy e |]
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
  , ( const2 "evaluate (lcCNAdd lcCN1 lcCN2) should be lcCN3 with unnormalized parts"
    , [qTerm| $lcCNAdd $lcCN1 $lcCN2 |]
    , [qTerm| @\r : * . \s : r -> r . \z : r . $lcCN1 @r s ($lcCN2 @r s z) |]
    )
  , ( const2 "evaluate (lcCNSucc lcCN1) should be lcCN2 with unnormalized parts"
    , [qTerm| $lcCNSucc $lcCN1 |]
    , [qTerm| @\r : * . \s : r -> r . \z : r . s ($lcCN1 @r s z) |]
    )
  , ( const2 "evaluate (lcCNPredDef lcCN5 lcCN1) should be lcCN0"
    , [qTerm| $lcCNPredDef $lcCN5 $lcCN1 |]
    , [qTerm| $lcCN0 |]
    )
  , ( const2 "evaluate (lcCNPredDef lcCN5 lcCN0) should be lcCN5"
    , [qTerm| $lcCNPredDef $lcCN5 $lcCN0 |]
    , [qTerm| $lcCN5 |]
    )
  , ( const2 "evaluate (lcCNIs0 lcCN0) should be lcCBTrue"
    , [qTerm| $lcCNIs0 $lcCN0 |]
    , [qTerm| $lcCBTrue |]
    )
  , ( const2 "evaluate (lcCNIs0 lcCN2) should be lcCBFalse"
    , [qTerm| $lcCNIs0 $lcCN2 |]
    , [qTerm| $lcCBFalse |]
    )
  , ( const2 "evaluate (lcCLHeadDef @lcCNTy lcCN3 lcCL_CN_0_1_2) should be lcCN0"
    , [qTerm| $lcCLHeadDef @$lcCNTy $lcCN3 $lcCL_CN_0_1_2 |]
    , [qTerm| $lcCN0 |]
    )
  , ( const2 "evaluate (lcCLTailDef lcCL_CN_0_1_2 (lcCLTailDef lcCL_CN_0_1_2 lcCL_CN_0)) should be lcCL_CN_0_1_2"
    , [qTerm| $lcCLTailDef @$lcCNTy $lcCL_CN_0_1_2 ($lcCLTailDef @$lcCNTy $lcCL_CN_0_1_2 $lcCL_CN_0) |]
    , [qTerm| $lcCL_CN_0_1_2 |]
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
  , ( const2 "normalize (lcCNAdd lcCN1 lcCN2) should be lcCN3"
    , [qTerm| $lcCNAdd $lcCN0 $lcCN1 |]
    , lcCN1
    )
  , ( const2 "normalize (lcCN4 lcBaseId) should be lcBaseId"
    , [qTerm| $lcCN4 @# $lcBaseId |]
    , lcBaseId
    )
  , ( const2 "normalize (lcCNPred lcCN4) should be lcCN3"
    , [qTerm| $lcCNPred $lcCN4 |]
    , lcCN3
    )
  , ( const2 "normalize (lcCNMinus lcCN5 lcCN3) should be lcCN2"
    , [qTerm| $lcCNMinus $lcCN5 $lcCN3 |]
    , lcCN2
    )
  , ( const2 "normalize (lcCNIs0 lcCN0) should be lcCBTrue"
    , [qTerm| $lcCNIs0 $lcCN0 |]
    , [qTerm| $lcCBTrue |]
    )
  , ( const2 "normalize (lcCNIs0 lcCN2) should be lcCBFalse"
    , [qTerm| $lcCNIs0 $lcCN2 |]
    , [qTerm| $lcCBFalse |]
    )
  , ( const2 "normalize (lcCLHeadDef lcCN3 lcCL_CN_0_1_2) should be lcCN0"
    , [qTerm| $lcCLHeadDef @$lcCNTy $lcCN3 $lcCL_CN_0_1_2 |]
    , [qTerm| $lcCN0 |]
    )
  , ( const2 "normalize (lcCLHeadDef lcCN3 (lcCLTail lcCL_CN_0_1_2)) should be lcCN1"
    , [qTerm| $lcCLHeadDef @$lcCNTy $lcCN3 ($lcCLTail @$lcCNTy $lcCL_CN_0_1_2) |]
    , [qTerm| $lcCN1 |]
    )
  , ( const2 "normalize (lcCLHeadDef lcCN3 (lcCLTail (lcCLTail lcCL_CN_0_1_2))) should be lcCN2"
    , [qTerm| $lcCLHeadDef @$lcCNTy $lcCN3 ($lcCLTail @$lcCNTy ($lcCLTail @$lcCNTy $lcCL_CN_0_1_2)) |]
    , [qTerm| $lcCN2 |]
    )
  , ( const2 "normalize (lcCLTail (lcCLTail (lcCLTail lcCL_CN_0_1_2))) should be lcCLNil"
    , [qTerm| $lcCLTail @$lcCNTy ($lcCLTail @$lcCNTy ($lcCLTail @$lcCNTy $lcCL_CN_0_1_2)) |]
    , [qTerm| $lcCLNil @$lcCNTy |]
    )
  , ( const2 "normalize (lcCLTailDef lcCL_CN_0_1_2 lcCL_CN_0) should be lcCLNil"
    , [qTerm| $lcCLTailDef @$lcCNTy $lcCL_CN_0_1_2 $lcCL_CN_0 |]
    , [qTerm| $lcCLNil @$lcCNTy |]
    )
  , ( const2 "normalize (lcCLTailDef lcCL_CN_0_1_2 (lcCLTailDef lcCL_CN_0_1_2 lcCL_CN_0)) should be lcCL_CN_0_1_2"
    , [qTerm| $lcCLTailDef @$lcCNTy $lcCL_CN_0_1_2 ($lcCLTailDef @$lcCNTy $lcCL_CN_0_1_2 $lcCL_CN_0) |]
    , [qTerm| $lcCL_CN_0_1_2 |]
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
