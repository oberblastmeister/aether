{-# LANGUAGE UndecidableInstances #-}

module Sexp.Parser
  ( Result,
    Parser,
    ListParser,
    SListParser,
    SParser,
    parseError',
    list,
    atom,
    item,
    ident,
    lit,
    parseRestUntil,
    listRest,
    runParser,
    assertAtListEnd,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Vector qualified as V
import Imports
import Sexp.Syntax

newtype Result m r = Result {getResult :: ReaderT Int (ExceptT ParseError m) r}
  deriving (Functor, Applicative, Monad)

instance (MonadState s m) => MonadState s (Result m) where
  get = Result get
  put = Result . put

type SParser m r = Parser m SexpPos r

type Parser m a r = a -> Result m r

data ListEnv r = ListEnv
  { pos :: Int,
    items :: Vector r
  }

newtype ListParser m r a = ListParser (ReaderT (ListEnv r) (StateT Int (ExceptT ParseError m)) a)
  deriving (Functor, Applicative, Monad)

type SListParser m = ListParser m SexpPos

data ParseError = ParseError Int Text
  deriving (Show, Eq)

class HasPos a where
  getPos :: a -> Int

instance HasPos SexpPos where
  getPos = (.ann)

runParser :: SParser m r -> SexpPos -> m (Either ParseError r)
runParser m s = runExceptT (runReaderT (m s).getResult s.ann)

withPos :: (Monad m) => Int -> Result m r -> Result m r
withPos pos p = Result do
  local (const pos) do
    p.getResult

parseError' :: (Monad m) => Int -> Text -> Result m a
parseError' pos err = Result do
  throwError $ ParseError pos err

instance (Monad m) => MonadError Text (Result m) where
  throwError err = Result do
    pos <- ask
    throwError $ ParseError pos err
  catchError = todo

instance (Monad m) => MonadError Text (ListParser m r) where
  throwError err = ListParser do
    env <- ask
    lift $ lift $ throwError (ParseError env.pos err)
  catchError = todo

parseRestUntil :: (Monad m, HasPos r) => Parser m r a -> ListParser m r Bool -> ListParser m r [a]
parseRestUntil p cond =
  cond >>= \case
    True -> pure []
    False -> do
      x <- item p
      xs <- parseRestUntil p cond
      pure $ x : xs

listRest :: (Monad m, HasPos r) => Parser m r a -> ListParser m r [a]
listRest p = parseRestUntil p atListEnd

atListEnd :: (Monad m) => ListParser m r Bool
atListEnd = ListParser do
  env <- ask
  i <- get
  pure $! i >= V.length env.items

assertAtListEnd :: (Monad m) => ListParser m r ()
assertAtListEnd = ListParser do
  env <- ask
  i <- get
  unless (i == V.length env.items) do
    throwError $ ParseError env.pos "did not consume the entire list".t

list :: (Monad m) => SListParser m a -> SParser m a
list m (List' pos xs) = withPos pos (runListParser m xs)
list _ (Atom' pos _) = parseError' pos "expected list".t

atom :: (Monad m) => Parser m Text a -> Parser m SexpPos a
atom p (Atom' pos t) = withPos pos (p t)
atom _ (List' pos _) = parseError' pos "expected atom".t

ident :: (Monad m) => Parser m SexpPos Text
ident = atom pure

lit :: (Monad m) => Text -> Parser m SexpPos ()
lit t = atom \case
  t' | t == t' -> pure ()
  t' -> throwError $ "expected literal ".t <> t <> " got ".t <> t'

runListParser :: (Monad m) => ListParser m r a -> Parser m [r] a
runListParser (ListParser p) sexps = Result do
  pos <- ask
  let res = evalStateT (runReaderT p (ListEnv pos (V.fromList sexps))) 0
  lift res

item :: (Monad m, HasPos r) => Parser m r a -> ListParser m r a
item p = ListParser do
  env <- ask
  i <- get
  case env.items ^? ix i of
    Nothing -> do
      lift $ lift do
        throwError $ ParseError env.pos ("expected " ++ show (i + 1) ++ " items").t
    Just x -> do
      put $! i + 1
      lift $ lift $ runReaderT ((p x).getResult) (getPos x)
