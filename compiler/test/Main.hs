module Main (main) where

import Cp qualified
import Data.Function ((&))
import System.FilePath
import System.Process.Typed qualified as Process
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

runtimePath = "compiler/test_data/test_runtime/zig-out/lib/libtest_runtime.a"

compileRuntime :: IO ()
compileRuntime = do
  compile
  -- Directory.doesFileExist runtimePath >>= \case
  --   False -> compile
  --   True -> pure ()
  where
    compile = do 
      Process.runProcess_
        ( Process.proc "zig" ["build"]
            & Process.setWorkingDir "compiler/test_data/test_runtime"
        )

tests :: TestTree
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
