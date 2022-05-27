module Unfork (unfork, Unfork (..), ShutdownBehavior (..)) where

import RunUntilException (runUntilException, ShutdownBehavior (..))
import TaskQueue (Task, doTask, enqueueTask)

import Prelude (IO, fmap)
import Control.Concurrent.STM (STM)

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM

data Unfork actionResult threadSafeAction
  where
    UnforkAsyncIO   :: Unfork a (IO (IO a))
    UnforkAsyncIO_  :: Unfork a (IO ())
    UnforkAsyncSTM  :: Unfork a (STM (STM a))
    UnforkAsyncSTM_ :: Unfork a (STM ())

unfork :: forall mb a b c.
    Unfork b mb
    -> ShutdownBehavior
    -> (a -> IO b)
        -- ^ Action that needs to be run from a single thread
    -> ((a -> mb) -> IO c)
        -- ^ Continuation that runs with a thread-safe version of the action
    -> IO c
unfork resultBehavior shutdownBehavior action continue =
    case resultBehavior of

        UnforkAsyncIO ->
            unfork UnforkAsyncSTM shutdownBehavior action \action' ->
            continue \x ->
            STM.atomically (fmap STM.atomically (action' x))

        UnforkAsyncIO_ ->
            unfork UnforkAsyncSTM_ shutdownBehavior action \action' ->
            continue \x ->
            STM.atomically (action' x)

        UnforkAsyncSTM -> do
            queue <- STM.newTQueueIO @(Task a b)
            Async.withAsync
                (runUntilException shutdownBehavior queue (doTask action))
                (\_ -> continue (enqueueTask queue))

        UnforkAsyncSTM_ -> do
            queue <- STM.newTQueueIO @a
            Async.withAsync
                (runUntilException shutdownBehavior queue action)
                (\_ -> continue (STM.writeTQueue queue))
