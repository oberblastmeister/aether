module Main where

import Args qualified
import Development.Shake
import Rules qualified
import System.Directory

main :: IO ()
main = do
  options <- Args.parse
  case options.directory of
    Nothing -> pure ()
    Just dir -> setCurrentDirectory dir
  shake
    ( shakeOptions
        { shakeColor = True,
          shakeFiles = Rules.outputDir,
          shakeChange = ChangeModtimeAndDigest
        }
    )
    $ Rules.rules options.command
