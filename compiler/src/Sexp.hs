module Sexp (module X) where

import Imports
import Sexp.Syntax as X

data SexpF a
  = AtomF Text
  | ListF [a]
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

newtype Fix f = Fix {unFix :: (f (Fix f))}

type Sexp = Fix SexpF

data SexpPosF a = SexpPos Int (SexpF a)

type SexpPos = Fix SexpPosF
