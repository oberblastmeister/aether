module CpTest where

import Cp qualified
import Data.Function ((&))
import SexpTest qualified
import System.FilePath
import System.Process.Typed qualified as Process
import Test.Tasty
import Test.Tasty.HUnit

runtimePath = "compiler/test_data/test_runtime/zig-out/lib/libtest_runtime.a"

compileRuntime :: IO ()
compileRuntime = do
  compile
  where
    -- Directory.doesFileExist runtimePath >>= \case
    --   False -> compile
    --   True -> pure ()

    compile = do
      Process.runProcess_
        ( Process.proc "zig" ["build"]
            & Process.setWorkingDir "compiler/test_data/test_runtime"
        )

tests =
  testGroup
    "cp"
    [ ( testCase "first" do
          runFile "first.cp"
      )
    ]

runFile path = do
  compileRuntime
  Cp.run "compiler/test_data" ("compiler/test_data" </> path) [runtimePath]
