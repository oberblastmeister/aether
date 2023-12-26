module Rules where

import Args qualified
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Development.Shake
import GHC.Stack (HasCallStack)
import System.Directory
import System.FilePath

ffiDir = "compiler/ffi"

buildFfi :: Action ()
buildFfi = do
  cmd_ (Cwd (ffiDir </> "zigbits")) "zig build"

ffiCabalPath = ffiDir </> "ffi.cabal"

ffiCabalTemplatePath = ffiDir </> "ffi.cabal.template"

rules :: (HasCallStack) => Args.Command -> Rules ()
rules command = do
  ffiCabalPath %> \p -> do
    need [ffiCabalTemplatePath]
    contents <- liftIO $ T.readFile ffiCabalTemplatePath
    path <- liftIO $ getCurrentDirectory
    let contents' =
          T.intercalate
            (T.pack "\n")
            [ contents,
              T.pack ("  extra-lib-dirs: " ++ (path </> ffiDir </> "zigbits/zig-out/lib")),
              T.pack "-- Do not edit! This file was generated!"
            ]
    liftIO $ T.writeFile p contents'

  action case command of
    Args.Build -> do
      need [ffiCabalPath]
      buildFfi
      cmd_ "cabal build"
    Args.Test args -> do
      need [ffiCabalPath]
      buildFfi
      cmd_ "cabal run tests" "--" args
    Args.Clean -> do
      putInfo "cleaning"
      removeFilesAfter outputDir ["//*"]
      removeFilesAfter ffiDir ["ffi.cabal"]
    _ -> do
      putInfo "not implemented yet"

outputDir = "_build"
