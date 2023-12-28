module Rules where

import Args qualified
import Data.List qualified as List
import Data.Text (Text)
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

compilerCabalPath = "compiler.cabal"

compilerTemplatePath = "compiler.cabal.template"

data TemplateVariables = TemplateVariables
  { extraLibDirs :: [FilePath]
  }

defaultTemplateVariables :: TemplateVariables
defaultTemplateVariables =
  TemplateVariables
    { extraLibDirs = []
    }

createCabalFromTemplate :: FilePath -> FilePath -> TemplateVariables -> Rules ()
createCabalFromTemplate templatePath outputPath vars = outputPath %> \p -> do
  need [templatePath]
  contents <- liftIO $ T.readFile templatePath
  path <- liftIO $ getCurrentDirectory
  let realExtraLibDirs = (path </>) <$> vars.extraLibDirs
  let replaced = replaceMany [(T.pack "{{extra-lib-dirs}}", T.pack (List.intercalate ", " realExtraLibDirs))] contents
  let contents' =
        T.intercalate
          (T.pack "\n")
          [ replaced,
            T.pack "-- Do not edit! This file was generated!"
          ]
  liftIO $ T.writeFile outputPath contents'

replaceMany :: [(Text, Text)] -> Text -> Text
replaceMany [] !t = t
replaceMany ((from, to) : rest) !t = replaceMany rest (T.replace from to t)

rules :: (HasCallStack) => Args.Command -> Rules ()
rules command = do
  createCabalFromTemplate
    ffiCabalTemplatePath
    ffiCabalPath
    ( defaultTemplateVariables
        { extraLibDirs = [ffiDir </> "zigbits/zig-out/lib"]
        }
    )
  -- ffiCabalPath %> \p -> do
  --   need [ffiCabalTemplatePath]
  --   contents <- liftIO $ T.readFile ffiCabalTemplatePath
  --   path <- liftIO $ getCurrentDirectory
  --   let contents' =
  --         T.intercalate
  --           (T.pack "\n")
  --           [ contents,
  --             T.pack ("  extra-lib-dirs: " ++ (path </> ffiDir </> "zigbits/zig-out/lib")),
  --             T.pack "-- Do not edit! This file was generated!"
  --           ]
  --   liftIO $ T.writeFile p contents'

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
