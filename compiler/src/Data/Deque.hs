module Data.Deque
  ( Deque,
    snoc,
    cons,
    unsnoc,
    uncons,
    empty,
    fromList,
  )
where

import Optics qualified as O

-- | A non persistent double ended queue.
data Deque a = Deque [a] [a]

fromList :: [a] -> Deque a
fromList xs = Deque xs []

snoc :: Deque a -> a -> Deque a
snoc (Deque front back) x = Deque front (x : back)

cons :: a -> Deque a -> Deque a
cons x (Deque front back) = Deque (x : front) back

unsnoc :: Deque a -> Maybe (Deque a, a)
unsnoc (Deque front back) = case back of
  [] -> case reverse front of
    [] -> Nothing
    x : back -> Just (Deque [] back, x)
  x : back -> Just (Deque front back, x)

uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque front back) = case front of
  [] -> case reverse back of
    [] -> Nothing
    x : front -> Just (x, Deque front [])
  x : front -> Just (x, Deque front back)

empty :: Deque a
empty = Deque [] []

instance O.Cons (Deque a) (Deque b) a b where
  _Cons =
    O.prism
      (uncurry cons)
      ( \dq -> case uncons dq of
          Nothing -> Left empty
          Just (x, dq) -> Right (x, dq)
      )

instance O.Snoc (Deque a) (Deque b) a b where
  _Snoc =
    O.prism
      (uncurry snoc)
      ( \dq -> case unsnoc dq of
          Nothing -> Left empty
          Just (dq, x) -> Right (dq, x)
      )
