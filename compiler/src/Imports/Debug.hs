module Imports.Debug
  ( traceM,
    traceId,
    pShow,
    pShowC,
    pPrint,
  )
where

import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Debug.Trace qualified as D
import Text.Pretty.Simple qualified as P

traceM :: (Monad m) => String -> m ()
traceM = D.traceM
{-# WARNING traceM "'traceM' is used" #-}

traceId :: String -> String
traceId = D.traceId
{-# WARNING traceId "'traceId' is used" #-}

pShow :: (Show a) => a -> TL.Text
pShow = P.pShowOpt P.defaultOutputOptionsNoColor {P.outputOptionsIndentAmount = 2}

pShowC :: (Show a) => a -> TL.Text
pShowC = P.pShowOpt P.defaultOutputOptionsDarkBg {P.outputOptionsIndentAmount = 2}

pPrint :: (Show a) => a -> IO ()
pPrint = P.pPrintOpt P.CheckColorTty P.defaultOutputOptionsDarkBg {P.outputOptionsIndentAmount = 2}
