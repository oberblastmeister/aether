module Snapshot where

import Control.Exception qualified as Exception
import Control.Monad
import Data.HashSet qualified as HashSet
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Imports
import System.Directory qualified as Fs
import System.FilePath
import System.IO qualified as IO
import System.IO.Temp qualified as Temp
import System.Process.Typed qualified as Proc
import Test.Tasty
import Test.Tasty.Golden.Advanced qualified as Golden

runDiff :: FilePath -> FilePath -> IO ()
runDiff p1 p2 = do
  let orOther m1 m2 = Exception.catch m1 $ \(e :: Exception.IOException) -> m2

  void $
    Proc.runProcess (Proc.proc "difft" [p1, p2, "--override", ".*:Haskell"])
      `orOther` Proc.runProcess (Proc.proc "git" ["diff", "--no-index", p1, p2])

myGoldenTest :: String -> FilePath -> FilePath -> (Text -> LText) -> TestTree
myGoldenTest name expectedPath codePath convertCode =
  Golden.goldenTest @Text
    name
    ( do
        Fs.doesFileExist expectedPath >>= \case
          False -> pure "".t
          True -> T.readFile expectedPath
    )
    ( do
        code <- T.readFile codePath
        pure $ TL.toStrict $ (convertCode code) <> "\n".tl
    )
    ( \expected actual -> do
        if expected /= actual
          then do
            Temp.withSystemTempFile "snapshot" $ \actualPath actualHandle -> do
              T.hPutStr actualHandle actual
              IO.hFlush actualHandle
              IO.hClose actualHandle
              Temp.withSystemTempFile "empty" $ \temp' tempHandle' -> do
                IO.hClose tempHandle'
                expectedPath <-
                  Fs.doesFileExist expectedPath >>= \case
                    False -> pure temp'
                    True -> pure expectedPath
                runDiff expectedPath actualPath
            pure $ Just "Snapshots did not match"
          else pure Nothing
    )
    (\t -> T.writeFile expectedPath t)

snapshotDir :: FilePath -> String -> (Text -> TL.Text) -> IO [TestTree]
snapshotDir dir extension convertCode = do
  files <- Fs.listDirectory dir
  files <- pure $ map (dir </>) files
  files <-
    filterM
      ( \path -> do
          doesExist <- Fs.doesFileExist path
          pure $ doesExist && takeExtension path == extension
      )
      files
  let fileSet = HashSet.toList . HashSet.fromList $ files
  pure $ map (\path -> myGoldenTest (takeBaseName path) (path -<.> "output") path convertCode) fileSet
