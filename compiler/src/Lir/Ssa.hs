module Lir.Ssa
  ( toSsa,
  )
where

import Cfg qualified
import Data.Str (Str)
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Imports
import Lir.Instr

-- the phi fact
-- it is Nothing when the argument is removed
data Phi = Phi
  { to :: Cfg.Name,
    from :: [Maybe Cfg.Name]
  }

type PhiFacts = Cfg.LabelMap [Phi]

toSsa :: Graph -> Graph
toSsa graph = todo
  where
    naiveSsa = toNaiveSsa graph

getPhiFacts graph = todo

toNaiveSsa :: Graph -> Graph
toNaiveSsa graph = renamedGraph
  where
    livenessFacts = runLiveness graph

    addBlockArgsStuff :: Cfg.Label -> Block -> Block
    addBlockArgsStuff label block = block {Cfg.entry, Cfg.exit}
      where
        live = (livenessFacts ^?! ix label) ^.. folded
        entry = BlockArgs (live ^.. folded)
        exit = block.exit & blockCalls %~ (\bc -> bc {args = Var <$> live})

    graphWithBlockArgs = graph & #blocks % itraversed `iover'` addBlockArgsStuff
    renamedGraph = renameGraph livenessFacts graphWithBlockArgs

-- mapping from name to generation
type RenameState = HashMap Str Int

type Liveness = (Cfg.LabelMap (Set Cfg.Name))

type E es = (Reader Liveness :> es, State RenameState :> es)

renameGraph :: Liveness -> Graph -> Graph
renameGraph livenessFacts graph =
  traverseOf (#blocks % traversed) renameBlock graph
    & evalState @RenameState mempty
    & runReader @Liveness livenessFacts
    & runPureEff

renameBlock :: (E es) => Block -> Eff es Block
renameBlock block = do
  entry <- rename block.entry
  body <- traverse rename block.body
  exit <- rename block.exit
  pure $ block {Cfg.entry, Cfg.body, Cfg.exit}

rename :: (E es) => Instr Operand c -> Eff es (Instr Operand c)
rename = renameUses >=> renameDefs

renameUses :: (E es) => Instr Operand c -> Eff es (Instr Operand c)
renameUses instr = do
  forOf usesTraversal instr \name -> do
    let str = Cfg.nameStr name
    gen <- gets @RenameState (^?! ix str)
    pure $ Cfg.GenName str gen

renameDefs :: (E es) => Instr Operand c -> Eff es (Instr Operand c)
renameDefs instr = do
  forOf defsTraversal instr \name -> do
    let str = Cfg.nameStr name
    gen <- gets @RenameState (^. at str % unwrapOr 0)
    modify @RenameState (at str ?~ gen + 1)
    pure $ Cfg.GenName str gen
