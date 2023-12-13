module Imports.Panic where

import GHC.Stack (HasCallStack)

todo :: (HasCallStack) => a
todo = error "TODO"
{-# WARNING todo "'todo' is used" #-}

todoMsg :: (HasCallStack) => String -> a
todoMsg msg = error $ "TODO: " ++ msg
{-# WARNING todoMsg "'todoMsg' is used" #-}
