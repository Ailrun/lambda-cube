module Main
  ( main
  ) where

import qualified LambdaCube.STLCTest     as STLC
import qualified LambdaCube.SystemFwTest as SystemFw
import           Test.Tasty

main :: IO ()
main = do
  testsForSTLC <- STLC.tests
  testsForSystemFw <- SystemFw.tests
  defaultMain $ testGroup "tests"
    [ testsForSTLC
    , testsForSystemFw
    ]
