module LirTest where

import Imports
import Lir.Sexp
import Sexp.Parser qualified as SP
import Sexp.Syntax qualified as S
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Lir"
    [
      -- ( testCase "first" do
      --     -- let s = "(let (y u64) (add (x u64) (y u64)))".t
      --     let s = "(label first (let (y u64) (add (x u64) (y u64))) (ret))".t
      --     let sexp = S.parse s ^?! _Right
      --     let blocks = traverse (SP.runParser pBlock) sexp
      --     putStrLn $ show blocks
      --     pure ()
      -- )
    ]
