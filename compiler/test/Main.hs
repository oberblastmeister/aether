module Main (main) where

import CpTest qualified
import Imports
import Lir.Sexp qualified
import LirTest qualified
import Sexp qualified
import Sexp.Parser qualified
import SexpTest qualified
import Snapshot qualified
import Test.Tasty

main :: IO ()
main = do
  lirSnapshots <-
    Snapshot.snapshotDir
      "compiler/test_data/lir_parse"
      ".lir"
      ( \t -> do
          let sexp = Sexp.parse t ^?! _Right
          let functions = traverse (Sexp.Parser.runParser Lir.Sexp.pFunction) sexp
          pShow functions
      )
  defaultMain $
    testGroup
      "tests"
      [ tests,
        testGroup "lir" lirSnapshots
      ]

tests =
  testGroup
    "tests"
    [ CpTest.tests,
      SexpTest.tests,
      LirTest.tests
    ]
