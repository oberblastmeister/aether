module Lir.Print where

import Cfg qualified
import Imports
import Lir.Instr qualified as Lir
import Prettyprinter qualified as P

type Doc = P.Doc Void

pGraph :: Lir.Graph -> Doc
pGraph = todo
