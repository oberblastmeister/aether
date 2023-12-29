module Main (main) where

import CpTest qualified
import Imports
import Lir.Sexp qualified
import LirTest qualified
import SexpTest qualified
import Snapshot qualified
import Test.Tasty

main :: IO ()
main = do
  lirSnapshots <- LirTest.snapshots
  defaultMain $
    testGroup
      "tests"
      [ tests,
        lirSnapshots
      ]

tests =
  testGroup
    "tests"
    [ CpTest.tests,
      SexpTest.tests,
      LirTest.tests
    ]
