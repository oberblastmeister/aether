module Effects.Fresh
  ( Fresh,
    runFresh,
    fresh,
  )
where

import Effectful
import Effectful.Dispatch.Static

data Fresh :: Effect

type instance DispatchOf Fresh = Static NoSideEffects

newtype instance StaticRep (Fresh) = Fresh Int

runFresh :: Eff (Fresh : es) a -> Eff es a
runFresh = evalStaticRep (Fresh 0)

fresh :: (Fresh :> es) => Eff es Int
fresh = do
  Fresh counter <- getStaticRep
  putStaticRep $ Fresh (counter + 1)
  pure counter
