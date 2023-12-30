{-# LANGUAGE ImpredicativeTypes #-}

module Cfg.Dataflow
  ( Transfer (..),
    TransferInstr (..),
    Direction,
    runTransfer,
    livenessTransfer,
    livenessInstrTransfer,
    dominatorsTransfer,
  )
where

import Cfg.Graph
import Cfg.Types
import Data.HashSetDeque (HashSetDeque)
import Data.HashSetDeque qualified as HashSetDeque
import Data.NonDet.Class (NonDetOrd)
import Data.NonDet.Set qualified as NDSet
import Data.Some (Some (..))
import Imports

-- A transfer terminates if the transfer function is monotone and the domain is finite.
-- This transfer works on block granularity
data Transfer i d = Transfer
  { -- a transfer is determined solely by the predecessor facts and the current block
    -- usually a function will combine the predecessor facts first and then use it
    -- with the current block to compute the new fact
    -- it is enough to compare the sizes of the current fact and the new fact to
    -- determine if the new fact has changed this is because the transfer function is monotone
    transfer ::
      -- label of block
      Label ->
      -- current block
      Block i ->
      -- predecessor/successor facts
      [d] ->
      -- current fact
      d ->
      -- returns a new fact only it is different from the current fact
      Maybe d,
    empty :: d,
    direction :: Direction
  }

data TransferInstr i d = TransferInstr
  { transfer ::
      forall c.
      -- current instruction
      i c ->
      -- previous fact
      d ->
      -- new fact
      d,
    changed ::
      -- current fact
      d ->
      -- new fact
      d ->
      Bool,
    -- empty fact
    empty :: d,
    -- combine facts from the predecessors or successors
    combine :: [d] -> d,
    direction :: Direction
  }

transferInstr :: TransferInstr i d -> Transfer i d
transferInstr transferInstr@TransferInstr {transfer = transferFn} =
  Transfer
    { transfer,
      empty = transferInstr.empty,
      direction = transferInstr.direction
    }
  where
    transfer _label block otherFacts currentFact =
      if transferInstr.changed currentFact newFacts
        then Just newFacts
        else Nothing
      where
        newFacts =
          foldlOf'
            ( case transferInstr.direction of
                Forward -> blockInstrsForward
                Backward -> blockInstrsBackward
            )
            (\prevFacts (Some instr) -> transferFn instr prevFacts)
            (transferInstr.combine otherFacts)
            block

data Direction = Forward | Backward

-- this is more of a database saturation algorithm
-- facts for predecessors/successors are always computed first in a bfs manner
-- most of the classical algorithms do a fixpoint algorithm with some fixed traversal
-- (reverse post order, etc.)
runTransfer :: forall i d. (HasJumps (i C)) => Transfer i d -> Graph i -> LabelMap d
runTransfer transfer graph = go initialFacts initialQueue
  where
    initialFacts = transfer.empty <$ graph.blocks
    initialQueue = HashSetDeque.fromList case transfer.direction of
      Forward -> [graph.start]
      Backward -> [graph.end]
    transpose = graphPrecessors graph
    go :: LabelMap d -> HashSetDeque Label -> LabelMap d
    go !factBase !queue = case HashSetDeque.uncons queue of
      Just (label, queue) -> case maybeNewFacts of
        Just newFact -> go (factBase & at' label ?~ newFact) newQueue
        Nothing -> do
          -- the facts for this label didn't change, so don't add its successors/predecessors to the queue
          go factBase queue
        where
          newQueue =
            foldl'
              HashSetDeque.snoc
              queue
              ( case transfer.direction of
                  Forward -> succs
                  Backward -> preds
              )
          maybeNewFacts = transfer.transfer label currentBlock otherFacts currentFact
          otherFacts = case transfer.direction of
            Forward -> predFacts
            Backward -> succFacts
          currentBlock = graph.blocks ^?! ix label
          currentFact = factBase ^?! ix label
          predFacts = (\l -> factBase ^?! ix l) <$> preds
          -- some labels may not be in transpose (for example start label)
          preds = transpose ^. at label % unwrapOr []
          succFacts = (\l -> factBase ^?! ix l) <$> succs
          succs = jumps currentBlock.exit
      Nothing -> factBase

type LivenessConstraint i n = (NonDetOrd n, forall c. HasDefs (i c) n, forall c. HasUses (i c) n)

livenessTransfer :: (LivenessConstraint i n) => Transfer i (NDSet.Set n)
livenessTransfer = transferInstr livenessInstrTransfer
{-# INLINEABLE livenessTransfer #-}

livenessInstrTransfer :: (LivenessConstraint i n) => TransferInstr i (NDSet.Set n)
livenessInstrTransfer =
  TransferInstr
    { transfer = \instr prevFacts ->
        (prevFacts `NDSet.difference` (NDSet.fromList (defs instr)))
          `NDSet.union` (NDSet.fromList (uses instr)),
      changed = \currentFact newFact -> NDSet.size newFact > NDSet.size currentFact,
      empty = NDSet.empty,
      combine = NDSet.unions,
      direction = Backward
    }
{-# INLINEABLE livenessInstrTransfer #-}

-- computes the labels that dominate a given label
-- probably need to invert if we want the domtree
dominatorsTransfer :: Transfer i (NDSet.Set Label)
dominatorsTransfer =
  Transfer
    { transfer,
      -- only works because we avoid intersections with empty set
      -- as all the predecessor facts are non-empty as they are calculated already
      -- normally has to be initialized with all the labels if using a lattice like algorithm
      empty = NDSet.empty,
      direction = Forward
    }
  where
    transfer label _block predFacts currFact =
      if NDSet.size newFact > NDSet.size currFact
        then Just newFact
        else Nothing
      where
        -- newFact is always nonempty, because we insert at the end
        newFact = NDSet.insert label (intersections predFacts)
        intersections (set : sets) = foldl' NDSet.intersection set sets
        intersections [] = NDSet.empty
