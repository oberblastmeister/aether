module Cp.Driver where

import Cp.Backend.C qualified as Backend.C
import Cp.Check qualified as Check
import Cp.Parser qualified as Parser
import Data.Text.IO qualified as T
import System.FilePath
import System.FilePath qualified as FilePath
import System.IO qualified as IO
import System.IO.Temp qualified as Temp
import System.Process.Typed qualified as Process

compileCFile :: FilePath -> FilePath -> IO ()
compileCFile fp executable = do
  Process.runProcess_ (Process.proc "zig" ["build-obj", "test_runtime.zig", "-lc"])
  Process.runProcess_ (Process.proc "zig" ["cc", "-std=c17", fp, "test_runtime.o", "-g", "-o", executable])

run :: FilePath -> IO ()
run path = do
  contents <- T.readFile path
  let syntax = Parser.parse contents
  case syntax of
    Left e -> putStrLn $ Parser.showError e
    Right syntax -> do
      putStrLn $ "syntax: " ++ show syntax
      putStrLn "checking"
      case Check.checkProgram syntax of
        Left e -> putStrLn $ show e
        Right (program, info) -> do
          let cOutput = Backend.C.genProgram info False program
          T.putStrLn $ "output:\n".t <> cOutput
          let cFile = path -<.> ".c"
          let exeFile = dropExtension path
          T.writeFile cFile cOutput
          compileCFile cFile exeFile
          Process.runProcess_ (Process.proc exeFile [])

-- Temp.withSystemTempFile "compile.c" \tempFp tempH -> do
--   T.hPutStr tempH cOutput
--   IO.hFlush tempH
--   IO.hClose tempH
