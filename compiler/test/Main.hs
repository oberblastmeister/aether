module Main (main) where

import Cp qualified
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "cp"
    [ ( testCase "first" do
          Cp.run "test_data/first.cp"
          pure ()
      ),
      ( testCase "second" do
          pure ()
      )
    ]
