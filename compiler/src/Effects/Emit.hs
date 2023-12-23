module Effects.Emit where

import Control.Monad.Primitive (RealWorld)
import Data.Primitive.ByteArray
import Effectful
import Effectful.Dispatch.Static
import Imports

data Emit :: Effect

type instance DispatchOf Emit = Static NoSideEffects

newtype instance StaticRep (Emit) = Emit {mutableByteArray :: MutableByteArray RealWorld}

