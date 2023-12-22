module Rules where

import Args qualified
import Development.Shake
import GHC.Stack (HasCallStack)

rules :: (HasCallStack) => Args.Command -> Rules ()
rules command = do
  action case command of
    -- Args.Build -> do
    --   undefined
    -- Args.Test -> do
    --   undefined
    _ -> do
      liftIO $ putStrLn "not implemented yet"

outputDir = "_build"
