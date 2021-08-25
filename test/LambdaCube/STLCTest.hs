{-# LANGUAGE QuasiQuotes #-}
module LambdaCube.STLCTest
  ( tests
  ) where

import           Control.Monad    (forM_)
import qualified Data.Text        as Text
import           LambdaCube.STLC
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.Hspec (testSpec)

tests :: IO TestTree
tests = testSpec "STLC tests" spec

spec :: Spec
spec = do
  describe "STLC infer" inferSpec
  describe "STLC evaluate" evaluateSpec
  describe "STLC normalize" normalizeSpec

inferSpec :: SpecWith ()
inferSpec = forM_ inferSpecCases $ \(extTm, extTy) -> do
  let
    tm = elaborate extTm
    ty = elaborateType extTy
    caseTitle =
      concat
      [ "infer ("
      , Text.unpack (prettyUnnamedTerm tm)
      , ") should be ("
      , Text.unpack (prettyUnnamedType ty)
      , ")"
      ]
  it caseTitle $ do
    infer tm `shouldBe` ty

inferSpecCases :: [(ExtLCTerm, ExtLCType)]
inferSpecCases =
  [ ( baseId
    , baseArr
    )
  , ( [qTerm| \x : # . (\x : $baseArr . x) $baseId x |]
    , baseArr
    )
  , ( [qTerm| \abc : # . (\fdd : $baseArr . fdd abc) |]
    , [qType| # -> $baseArr -> # |]
    )
  , ( pCN3
    , pCNTy
    )
  , ( pCNAdd
    , [qType| $pCNTy -> $pCNTy -> $pCNTy |]
    )
  , ( [qTerm| $pCNAdd $pCN1 $pCN2 |]
    , [qType| $pCNTy |]
    )
  , ( pCNMul
    , [qType| $pCNTy -> $pCNTy -> $pCNTy |]
    )
  , ( [qTerm| $pCNMul $pCN0 |]
    , [qType| $pCNTy -> $pCNTy |]
    )
  ]
  where
    baseId = [qTerm| \x : # . x |]
    baseArr = [qType| # -> # |]

    -- Pseudo Church Numeral tests
    pCNTy = [qType| $baseArr -> $baseArr |]

    pCN0 = [qTerm| \s : $baseArr . $baseId |]
    pCN1 = [qTerm| \s : $baseArr . \z : # . s z |]
    pCN2 = [qTerm| \s : $baseArr . \z : # . s (s z) |]
    pCN3 = [qTerm| \s : $baseArr . \z : # . s (s (s z)) |]
    pCNAdd =
      [qTerm|
         \n : $pCNTy . \m : $pCNTy .
         \s : $baseArr . \z : # . n s (m s z)
      |]
    pCNMul =
      [qTerm|
         \n : $pCNTy . \m : $pCNTy .
         \s : $baseArr . \z : # . n (m s) z
      |]

evaluateSpec :: SpecWith ()
evaluateSpec = forM_ evaluateSpecCases $ \(extTm, extResTm) -> do
  let
    tm = elaborate extTm
    resTm = elaborate extResTm
    caseTitle =
      concat
      [ "liftLCValue (evaluate ("
      , Text.unpack (prettyUnnamedTerm tm)
      , ")) should be ("
      , Text.unpack (prettyUnnamedTerm resTm)
      , ")"
      ]
  it caseTitle $ do
    liftLCValue (evaluate tm) `shouldBe` resTm

evaluateSpecCases :: [(ExtLCTerm, ExtLCTerm)]
evaluateSpecCases = []

normalizeSpec :: SpecWith ()
normalizeSpec = forM_ normalizeSpecCases $ \(extTm, extResTm) -> do
  let
    tm = elaborate extTm
    resTm = elaborate extResTm
    caseTitle =
      concat
      [ "liftLCValue (normalize ("
      , Text.unpack (prettyUnnamedTerm tm)
      , ")) should be ("
      , Text.unpack (prettyUnnamedTerm resTm)
      , ")"
      ]
  it caseTitle $ do
    liftLCNormal (normalize tm) `shouldBe` resTm

normalizeSpecCases :: [(ExtLCTerm, ExtLCTerm)]
normalizeSpecCases = []
