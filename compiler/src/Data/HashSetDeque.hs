module Data.HashSetDeque
  ( HashSetDeque,
    uncons,
    snoc,
    empty,
    fromList,
  )
where

import Data.Deque (Deque)
import Data.Deque qualified as Deque
import Data.HashSet qualified as HS
import Imports hiding (cons, snoc, uncons)

data HashSetDeque a = HashSetDeque (HashSet a) (Deque a)

uncons :: (Hashable a) => HashSetDeque a -> Maybe (a, HashSetDeque a)
uncons (HashSetDeque set xs) = case Deque.uncons xs of
  Just (x, xs) -> Just (x, HashSetDeque (set & at x .~ Nothing) xs)
  Nothing -> Nothing

snoc :: (Hashable a) => HashSetDeque a -> a -> HashSetDeque a
snoc (HashSetDeque set xs) x = HashSetDeque (set & at x ?~ ()) (Deque.snoc xs x)

empty :: HashSetDeque a
empty = HashSetDeque HS.empty Deque.empty

fromList :: (Hashable a) => [a] -> HashSetDeque a
fromList xs = HashSetDeque (HS.fromList xs) (Deque.fromList xs)
