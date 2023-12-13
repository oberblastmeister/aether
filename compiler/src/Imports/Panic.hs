module Imports.Panic where

import GHC.Stack (HasCallStack)

todo :: (HasCallStack) => a
todo = error "TODO"
{-# WARNING todo "'todo' is used" #-}
