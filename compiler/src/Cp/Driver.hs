module Cp.Driver where

import Cp.Backend.C qualified as Backend.C
import Cp.Check qualified as Check
import Cp.Parser qualified as Parser
import Data.Text.IO qualified as T
import Imports
import System.Directory qualified as Directory
import System.FilePath
import System.Process.Typed qualified as Process

compileCFile :: FilePath -> FilePath -> [FilePath] -> IO ()
compileCFile fp executable link = do
  Process.runProcess_ (Process.proc "zig" $ ["cc", "-std=c17", fp, "-g", "-o", executable] ++ link)
  -- Process.runProcess_ (Process.proc "clang" $ ["-std=c17", fp, "-g", "-o", executable] ++ link)

data RunOptions = RunOptions
  { dir :: FilePath,
    path :: FilePath,
    runtime :: FilePath
  }

run :: FilePath -> FilePath -> [FilePath] -> IO ()
run dir path link = do
  contents <- T.readFile path
  let syntax = Parser.parse contents
  case syntax of
    Left e -> putStrLn $ Parser.showError e
    Right syntax -> do
      putStrLn $ "syntax: " ++ pShowC syntax
      putStrLn "checking"
      case Check.checkProgram syntax of
        Left e -> putStrLn $ show e
        Right program -> do
          let buildDir = dir </> "_build"
          Directory.createDirectoryIfMissing False buildDir
          let cOutput = Backend.C.genProgram False program
          T.putStrLn $ "output:\n".t <> cOutput
          let name = takeFileName path
          let cFile = buildDir </> name -<.> ".c"
          T.writeFile cFile cOutput
          let exeFile = buildDir </> name -<.> ""
          compileCFile cFile exeFile link
          Process.runProcess_ (Process.proc exeFile [])
