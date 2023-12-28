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

newtype Result r = Result {getResult :: ReaderT Int (Either ParseError) r}
  deriving (Functor, Applicative, Monad)

type SParser r = Parser SexpPos r

type Parser a r = a -> Result r

data ListEnv r = ListEnv
  { pos :: Int,
    items :: Vector r
  }

newtype ListParser r a = ListParser (ReaderT (ListEnv r) (StateT Int (Either ParseError)) a)
  deriving (Functor, Applicative, Monad)

type SListParser = ListParser SexpPos

data ParseError = ParseError Int Text
  deriving (Show, Eq)

class HasPos a where
  getPos :: a -> Int

instance HasPos SexpPos where
  getPos = (.ann)

runParser :: SParser r -> SexpPos -> Either ParseError r
runParser m s = runReaderT (m s).getResult s.ann

withPos :: Int -> Result r -> Result r
withPos pos p = Result do
  local (const pos) do
    p.getResult

parseError' :: Int -> Text -> Result a
parseError' pos err = Result do
  throwError $ ParseError pos err

instance MonadError Text Result where
  throwError err = Result do
    pos <- ask
    throwError $ ParseError pos err
  catchError = todo

instance MonadError Text (ListParser r) where
  throwError err = ListParser do
    env <- ask
    lift $ lift $ throwError (ParseError env.pos err)
  catchError = todo

parseRestUntil :: (HasPos r) => Parser r a -> ListParser r Bool -> ListParser r [a]
parseRestUntil p cond =
  cond >>= \case
    True -> pure []
    False -> do
      x <- item p
      xs <- parseRestUntil p cond
      pure $ x : xs

listRest :: (HasPos r) => Parser r a -> ListParser r [a]
listRest p = parseRestUntil p atListEnd

atListEnd :: ListParser r Bool
atListEnd = ListParser do
  env <- ask
  i <- get
  pure $! i >= V.length env.items

assertAtListEnd :: ListParser r ()
assertAtListEnd = ListParser do
  env <- ask
  i <- get
  unless (i == V.length env.items) do
    throwError $ ParseError env.pos "did not consume the entire list".t

list :: SListParser a -> SParser a
list m (List' pos xs) = withPos pos (runListParser m xs)
list _ (Atom' pos _) = parseError' pos "expected list".t

atom :: Parser Text a -> Parser SexpPos a
atom p (Atom' pos t) = withPos pos (p t)
atom _ (List' pos _) = parseError' pos "expected atom".t

ident :: Parser SexpPos Text
ident = atom pure

lit :: Text -> Parser SexpPos ()
lit t = atom \case
  t' | t == t' -> pure ()
  t' -> throwError $ "expected literal ".t <> t <> " got ".t <> t'

runListParser :: ListParser r a -> Parser [r] a
runListParser (ListParser p) sexps = Result do
  pos <- ask
  let res = evalStateT (runReaderT p (ListEnv pos (V.fromList sexps))) 0
  lift res

item :: (HasPos r) => Parser r a -> ListParser r a
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
