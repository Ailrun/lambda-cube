module Main
  ( main
  ) where

import qualified LambdaCube.STLCTest as STLC
import           Test.Tasty

main :: IO ()
main = do
  testsForSTLC <- STLC.tests
  defaultMain $ testGroup "tests"
    [ testsForSTLC
    ]
