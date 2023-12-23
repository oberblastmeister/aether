module Imports.Monad
  ( module X,
    whenP,
  )
where

import Control.Monad as X (guard, unless, when, (<$!>), (<=<), (>=>))
-- import Control.Monad.Except as X (Except, ExceptT, MonadError, runExcept, runExceptT)
-- import Control.Monad.Reader as X (MonadReader (..), Reader, ReaderT, runReader, runReaderT)
-- import Control.Monad.State.Strict as X (MonadState (..), State, StateT, runState, runStateT)
-- import Control.Monad.Writer.CPS as X (MonadWriter (..), Writer, WriterT, runWriter)
import Optics

whenP :: (Monad m, Is k An_AffineFold) => Optic' k is s a -> s -> (a -> m ()) -> m ()
whenP o s f = do
  case s ^? o of
    Nothing -> pure ()
    Just a -> f a
{-# INLINE whenP #-}
