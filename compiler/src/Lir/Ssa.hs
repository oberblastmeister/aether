{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Lir.Ssa
  ( toSsa,
  )
where

import Cfg qualified
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Str (NonDetStr (..))
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Imports
import Lir.Instr
import Lir.Instr qualified as Lir

data PhiValue = PhiValue
  { label :: Cfg.Label,
    dest :: Cfg.Name,
    value :: Lir.Operand
  }

data Phi = Phi
  { label :: Cfg.Label,
    dest :: Cfg.Name,
    -- should be nonempty
    flowValues :: [PhiValue]
  }

makeFieldLabelsNoPrefix ''PhiValue
makeFieldLabelsNoPrefix ''Phi

toSsa :: Graph -> Graph
toSsa graph = graph'
  where
    graph' = substGraph subst naiveSsaGraph & putPhis simplifiedPhis
    (subst, simplifiedPhis) = simplifyPhis phis
    phis = getPhis naiveSsaGraph ^.. folded % folded
    naiveSsaGraph = toNaiveSsa graph

isPhiRemovable :: Phi -> Maybe PhiSubst
isPhiRemovable phi = do
  guard $ allSame others
  rep <- others ^? _head
  pure [(phi.dest, rep)].hm
  where
    others =
      filter
        (/= Var (phi.dest))
        (phi ^.. #flowValues % folded % #value)

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (== x) xs

type PhiSubst = (HashMap Cfg.Name Lir.Operand)

-- does not substitute entry or exit, because we will replace them anyway with putPhis
substGraph :: PhiSubst -> Graph -> Graph
substGraph subst graph = graph & #blocks % each % #body % each %~ substInstr subst

substInstr :: PhiSubst -> Instr Lir.Operand c -> Instr Lir.Operand c
substInstr subst instr = instr & Lir.instrOperands %~ substOperand subst

substOperand :: PhiSubst -> Lir.Operand -> Lir.Operand
substOperand subst (Var name) = subst ^. at name % unwrapOr (Var name)
substOperand _ operand = operand

substPhi :: PhiSubst -> Phi -> Phi
substPhi subst phi = phi & #flowValues % each % #value %~ substOperand subst

composeSubst :: PhiSubst -> PhiSubst -> PhiSubst
composeSubst s1 s2 = (substOperand s1) <$> s2

simplifyPhis :: [Phi] -> (PhiSubst, [Phi])
simplifyPhis = fixpoint mempty
  where
    fixpoint :: PhiSubst -> [Phi] -> (PhiSubst, [Phi])
    fixpoint !subst !phis = case tryRemove subst phis of
      (False, subst, phis) -> (subst, phis)
      (True, subst, phis) -> fixpoint subst phis

    tryRemove :: PhiSubst -> [Phi] -> (Bool, PhiSubst, [Phi])
    tryRemove !subst [] = (False, subst, [])
    tryRemove !subst (phi : phis) = case isPhiRemovable (substPhi subst phi) of
      Nothing -> tryRemove subst phis & _3' %!~ (phi :)
      Just subst' -> (True, composeSubst subst' subst, phis)

putPhis :: [Phi] -> Graph -> Graph
putPhis phis graph = graph'
  where
    graph' = graph & #blocks % itraversed `iover'` putPhisBlock
    putPhisBlock :: Cfg.Label -> Block -> Block
    putPhisBlock label block = block & #entry .~ BlockArgs blockArgs & #exit % blockCalls %!~ modifyBlockCall
      where
        modifyBlockCall blockCall = blockCall & #args .~ (grouped ^?! ix blockCall.label % ix label)
        blockArgs = blockArgsMap ^?! ix label
    -- mapping from label jumped to label jumped from to operands for block args
    grouped = (fmap . fmap) (fmap (.value) . List.sortBy (Cfg.compareName `on` (.dest))) $ HM.fromListWith (HM.unionWith (++)) do
      phi <- phis
      value <- phi.flowValues
      pure (phi.label, HM.fromList [(value.label, [value])])
    blockArgsMap =
      fmap (List.sortBy Cfg.compareName) $ HM.fromListWith (++) do
        phi <- phis
        pure (phi.label, [phi.dest])

getPhis :: Graph -> Cfg.LabelMap [Phi]
getPhis graph = res
  where
    res = foldlOf' (#blocks % each % #exit % blockCalls) go initialPhis graph
    go :: Cfg.LabelMap [Phi] -> Lir.BlockCall Lir.Operand -> Cfg.LabelMap [Phi]
    go blockPhis blockCall =
      blockPhis
        & at blockCall.label
        %~ (Just . \m -> addPhis blockCall.label (m ^?! _Just) blockCall.args)
    addPhis :: Cfg.Label -> [Phi] -> [Lir.Operand] -> [Phi]
    addPhis label = zipWith (\phi value -> phi & #flowValues %~ (PhiValue {label, dest = phi.dest, value} :))
    initialPhis = graph.blocks & itraversed `iover'` makePhis
      where
        makePhis label block =
          block.entry
            ^.. _BlockArgs
            % each
            <&> (\arg -> Phi {label, dest = arg, flowValues = []})

toNaiveSsa :: Graph -> Graph
toNaiveSsa graph = renamedGraph
  where
    livenessFacts = runLiveness graph
    addBlockArgsStuff :: Cfg.Label -> Block -> Block
    addBlockArgsStuff label block = block {Cfg.entry, Cfg.exit}
      where
        live =
          (livenessFacts ^?! ix label)
            ^.. folded
            & List.sortBy Cfg.compareName
        entry = BlockArgs (live ^.. folded)
        exit = block.exit & blockCalls %~ (\bc -> bc {args = Var <$> live})
    graphWithBlockArgs = graph & #blocks % itraversed `iover'` addBlockArgsStuff
    renamedGraph = renameGraph livenessFacts graphWithBlockArgs

-- mapping from name to generation
type RenameState = HashMap NonDetStr Int

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
