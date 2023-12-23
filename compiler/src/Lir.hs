{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

#include "effectful.h"
#include "effectful.h"

module Lir where

import Data.Text (Text)
import Effectful
import Effectful.State.Static.Local
import Effectful.Reader.Static
import Optics

data MyState = MyState
  { first :: String,
    second :: Text,
    third :: Bool
  }

makeFieldLabelsNoPrefix ''MyState

state (MyState)
reader (MyState)

testing :: (Reader' :> es, State' :> es) => Eff es ()
testing = do
  first <- use' #third
  testing <- get'
  #third .= True
  another <- ask'
  pure ()
