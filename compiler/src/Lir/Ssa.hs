{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Lir.Ssa
  ( toSsa,
    toNaiveSsa,
  )
where

import Cfg qualified
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.NonDet.Set qualified as NDSet
import Data.Str (Str)
import Effectful
import Effectful.Reader.Static
import Effectful.State.Static.Local
import Imports
import Lir.Instr
import Lir.Instr qualified as Lir

data PhiValue = PhiValue
  { -- the label where this value is passed in to a blockcall
    flowedFromLabel :: Cfg.Label,
    dest :: Value,
    value :: Value
  }
  deriving (Show)

data Phi = Phi
  { -- the label of the block argument for this phi
    destLabel :: Cfg.Label,
    dest :: Value,
    -- should be nonempty
    flowValues :: [PhiValue]
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''PhiValue
makeFieldLabelsNoPrefix ''Phi

toSsa :: Function Value -> Function Value
toSsa fn = naiveSsaFn {graph = graph'}
  where
    graph' =
      putPhis (substPhi subst <$> simplifiedPhis) naiveSsaFn.graph
        & substGraph subst
    -- invariant: the domain of the substitution should be a subseteq of the phi destinations
    (subst, simplifiedPhis) = simplifyPhis phis
    phis = getPhis naiveSsaFn.graph ^.. folded % folded
    naiveSsaFn = toNaiveSsa fn

isPhiRemovable :: Phi -> Maybe PhiSubst
isPhiRemovable phi = do
  guard $ allSame others
  rep <- others ^? _head
  pure [(phi.dest, rep)].hm
  where
    others = phi ^.. #flowValues % folded % #value % filtered (/= phi.dest)

allSame :: (Eq a) => [a] -> Bool
allSame [] = True
allSame (x : xs) = all (== x) xs

type PhiSubst = (HashMap Value Value)

substGraph :: PhiSubst -> Graph -> Graph
substGraph subst graph = graph & #blocks % each %~ (Cfg.mapBlock (substInstr subst))

substInstr :: PhiSubst -> Instr Value c -> Instr Value c
substInstr subst instr = instr & Lir.instrUses %~ substValue subst

substValue :: PhiSubst -> Value -> Value
substValue subst val = case subst ^. at val of
  Nothing -> val
  Just newVal -> newVal

substPhi :: PhiSubst -> Phi -> Phi
substPhi subst phi = phi & #flowValues % each % #value %~ substValue subst

composeSubst :: PhiSubst -> PhiSubst -> PhiSubst
composeSubst s1 s2 = s1 <> (fmap (substValue s1) s2)

-- this is basically just unification
-- might be better to use a mutable union-find data structure
simplifyPhis :: [Phi] -> (PhiSubst, [Phi])
simplifyPhis = fixpoint mempty
  where
    fixpoint :: PhiSubst -> [Phi] -> (PhiSubst, [Phi])
    fixpoint !subst !phis = case tryRemove subst phis of
      (False, subst, phis) -> (subst, phis)
      (True, subst, phis) -> fixpoint subst phis

    tryRemove :: PhiSubst -> [Phi] -> (Bool, PhiSubst, [Phi])
    tryRemove !subst [] = (False, subst, [])
    tryRemove !subst (phi : phis) =
      case isPhiRemovable (substPhi subst phi) of
        Nothing ->
          tryRemove subst phis & _3' %!~ (phi :)
        Just subst' ->
          (True, composeSubst subst' subst, phis)
      where

putPhis :: [Phi] -> Graph -> Graph
putPhis phis graph = graph'
  where
    graph' = graph & #blocks % itraversed `iover'` putPhisBlock
    putPhisBlock :: Cfg.Label -> Block -> Block
    putPhisBlock label block = block & #entry .~ BlockArgs blockArgs & #exit % blockCalls %!~ modifyBlockCall
      where
        modifyBlockCall blockCall = blockCall & #args .~ callArgs
          where
            callArgs = case grouped ^. at blockCall.label of
              Nothing -> []
              Just flowedFromLabelToValues -> flowedFromLabelToValues ^. at label % unwrapOr (error "should be called at least once because we only inserted phis for live values")
        -- there may not be any phis at the label, which means it should have empty arguments
        blockArgs = blockArgsMap ^. at label % non []

    -- mapping from label jumped to label jumped from to operands for block args
    -- first label checks if there are block arguments
    -- if there are, lookup through the second label should not fail
    grouped = (fmap . fmap) (fmap (.value) . List.sortOn (.dest)) $ HM.fromListWith (HM.unionWith (++)) do
      phi <- phis
      value <- phi.flowValues
      pure (phi.destLabel, HM.fromList [(value.flowedFromLabel, [value])])

    blockArgsMap :: HashMap Cfg.Label [Value]
    blockArgsMap =
      fmap List.sort $ HM.fromListWith (++) do
        phi <- phis
        pure (phi.destLabel, [phi.dest])

getPhis :: Graph -> Cfg.LabelMap [Phi]
getPhis graph = res
  where
    res = ifoldlOf' (#blocks % each % #exit % blockCalls) accumulatePhis initialPhis graph

    accumulatePhis :: Cfg.Label -> Cfg.LabelMap [Phi] -> Lir.BlockCall Value -> Cfg.LabelMap [Phi]
    accumulatePhis flowedFromLabel blockPhis blockCall =
      blockPhis
        & at destLabel
        %~ (Just . \m -> zipPhisWithValues flowedFromLabel (m ^?! _Just) blockCall.args)
      where
        destLabel = blockCall.label

    -- for a block arg like (label (my_block arg1 arg2))
    --
    -- if we call it like this:
    -- (jump (my_block val1 val2))
    --
    -- we will call zipPhisWithValues "my_block" [phi [...], phi [...]] [val1, val2]
    -- which will add val1 to the first phi's [...] and add val2 to the second phi's [...]
    zipPhisWithValues :: Cfg.Label -> [Phi] -> [Value] -> [Phi]
    zipPhisWithValues flowedFromLabel = zipWith \phi value ->
      phi & #flowValues %~ (PhiValue {flowedFromLabel, dest = phi.dest, value} :)

    -- for a block arg like (label (my_block arg1 arg2))
    -- creates phis:
    -- arg1 <- phi []
    -- arg2 <- phi []
    -- we will accumulate the flowValues above
    initialPhis = graph.blocks & itraversed `iover'` emptyPhiForEachBlockArg
      where
        emptyPhiForEachBlockArg destLabel block = block.entry ^.. _BlockArgs % each <&> emptyPhiForBlockArg
          where
            emptyPhiForBlockArg arg = Phi {destLabel, dest = arg, flowValues = []}

toNaiveSsa :: Function Value -> Function Value
toNaiveSsa fn = renamedFn
  where
    livenessFacts = runLiveness graph
    livenessFactsList = NDSet.toListOrd <$> livenessFacts
    addBlockArgsStuff :: Cfg.Label -> Block -> Block
    addBlockArgsStuff label block = block {Cfg.entry, Cfg.exit}
      where
        entry =
          BlockArgs
            ( if label == graph.start
                then []
                else (livenessFactsList ^?! ix label)
            )
        exit = block.exit & blockCalls %~ (\blockCall -> blockCall {args = livenessFactsList ^?! ix (blockCall.label)})
    graphWithBlockArgs = graph & #blocks % itraversed `iover'` addBlockArgsStuff
    renamedFn =
      runPureEff
        . runReader @Liveness livenessFacts
        . evalState @RenameState mempty
        $ do
          params <- traverse renameValueDef fn.params
          graph <- renameGraph graphWithBlockArgs
          pure $ fn {params, graph}
    graph = fn.graph

-- mapping from name to generation
type RenameState = HashMap Str Int

type Liveness = (Cfg.LabelMap (NDSet.Set Value))

type E es = (Reader Liveness :> es, State RenameState :> es)

renameGraph :: (E es) => Graph -> Eff es Graph
renameGraph graph =
  -- the order is important!
  -- we need to make sure that the start block sees the renames
  -- we performed on the parameters
  Cfg.traverseBlockOrderM (const renameBlock) graph

renameBlock :: (E es) => Block -> Eff es Block
renameBlock block = do
  entry <- rename block.entry
  body <- traverse rename block.body
  exit <- rename block.exit
  pure $ block {Cfg.entry, Cfg.body, Cfg.exit}

rename :: (E es) => Instr Value c -> Eff es (Instr Value c)
rename = renameUses >=> renameDefs

renameUses :: (E es) => Instr Value c -> Eff es (Instr Value c)
renameUses instr = do
  forOf instrUses instr \val -> do
    let str = showName val.name
    gen <- gets @RenameState (^?! ix str)
    pure $ val & #name .~ Cfg.GenName str gen

renameDefs :: (E es) => Instr Value c -> Eff es (Instr Value c)
renameDefs instr = forOf instrDefs instr renameValueDef

renameValueDef :: (E es) => Value -> Eff es Value
renameValueDef val = do
  let str = showName val.name
  modify @RenameState (at str %~ (Just . (+ 1) . fromMaybe (-1)))
  gen <- gets @RenameState (^. at str % unwrapOr 0)
  pure $ val & #name .~ Cfg.GenName str gen

showName :: Cfg.Name -> Str
showName (Cfg.StrName {str}) = str
showName (Cfg.GenName {str, gen}) = (str.t <> ".".t <> (show gen).t).str
