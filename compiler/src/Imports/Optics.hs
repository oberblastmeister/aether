module Imports.Optics
  ( module X,
    makeLenses',
    unwrapOr,
    expect,
  )
where

import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Optics as X hiding (pattern (:<), pattern (:>))
import Optics.Operators.Unsafe as X

makeLenses' = makeLensesWith underscoreFields

unwrapOr :: a -> Lens (Maybe a) (Maybe b) a b
unwrapOr def =
  lens
    (fromMaybe def)
    (\_ x -> Just x)
{-# INLINE unwrapOr #-}

expect :: (HasCallStack) => String -> Maybe a -> a
expect msg = fromMaybe (error msg)
{-# INLINE expect #-}
