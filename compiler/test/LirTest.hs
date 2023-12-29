module LirTest where

import Imports
import Lir.Elaborate qualified
import Lir.Instr qualified
import Lir.Instr qualified as Lir
import Lir.Sexp
import Lir.Ssa qualified
import Snapshot qualified
import Test.Tasty

snapshots =
  testGroup "lir snapshots" <$> do
    parseSnap <-
      testGroup "lir_parse" <$> do
        Snapshot.snapshotDir
          "compiler/test_data/lir_parse"
          ".lir"
          ( \t -> do
              let functions = Lir.Sexp.parseLir t
              pShow functions
          )
    livenessSnap <-
      testGroup "lir_liveness" <$> do
        Snapshot.snapshotDir
          "compiler/test_data/lir_liveness"
          ".lir"
          ( \t -> pShow do
              functions <- Lir.Sexp.parseLir t
              functions <- traverse Lir.Elaborate.elabFunction functions
              pure $ (fmap (Lir.Instr.runLiveness . (.graph)) functions)
          )
    ssaSnap <-
      testGroup "lir_ssa" <$> do
        Snapshot.snapshotDir
          "compiler/test_data/lir_ssa"
          ".lir"
          ( \t -> pShow do
              functions <- Lir.Sexp.parseLir t
              functions <- traverse Lir.Elaborate.elabFunction functions
              pure $ functions & each % #graph %~ Lir.Ssa.toNaiveSsa
          )
    pure [parseSnap, livenessSnap, ssaSnap]

tests :: TestTree
tests =
  testGroup
    "Lir"
    []
