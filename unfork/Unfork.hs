module Unfork
  (
    -- * Asynchronous I/O
    unforkAsyncIO,
    unforkAsyncIO_,
    Future (..),
    -- * Asynchronous STM
    unforkAsyncSTM,
    unforkAsyncSTM_,
    -- * Synchronous I/O
    unforkSyncIO,
    unforkSyncIO_,
  ) where

import Prelude (IO, Bool (..), Maybe (..), pure)

import Control.Applicative ((<|>))

import Control.Exception.Safe (bracket)

import Control.Monad (guard)

import Control.Monad.STM (STM, atomically)

import Control.Concurrent.Async (concurrently)

import Control.Concurrent.MVar
    (newEmptyMVar, newMVar, putMVar,
     readMVar, takeMVar, tryReadMVar)

import Control.Concurrent.STM.TQueue
    (TQueue, newTQueueIO, readTQueue, writeTQueue)

import Control.Concurrent.STM.TVar
    (TVar, newTVar, newTVarIO, readTVar, writeTVar)


{- ━━━━━  STM, asynchronous, with task results discarded  ━━━━━━━━━

    Discarding results makes this function simpler than those that
    make results available. All we do is maintain a queue of tasks,
    and each step of the queue loop runs the action.

-}

-- | Turns an IO action into a fire-and-forget STM action
unforkAsyncSTM_ ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> STM ()) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkAsyncSTM_ action =
    unforkAsync Unfork{ threadSafeAction, step }
  where
    threadSafeAction run arg = enqueue run arg
    step a = do{ _ <- action a; pure () }


{- ━━━━━  I/O, asynchronous, with task results discarded  ━━━━━━━━━

    This is just the same as its STM equivalent, but with an
    'atomically' thrown in for convenience.

-}

-- | Turns an IO action into a fire-and-forget async action
--
-- For example, use @('unforkAsyncIO_' 'putStrLn')@ to log to stdout in a multi-threaded application.
unforkAsyncIO_ ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> IO ()) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkAsyncIO_ action =
    unforkAsync Unfork{ threadSafeAction, step }
  where
    threadSafeAction run arg = atomically (enqueue run arg)
    step a = do{ _ <- action a; pure () }


{- ━━━━━  STM, asynchronous, with task results available  ━━━━━━━━━

    To make task results available, we maintain a queue that
    contains not only each the task itself, but also a TVar to
    store its result. Each step of the queue loop runs the action
    and then places the result into the TVar.

-}

unforkAsyncSTM ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> STM (STM (Maybe result))) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkAsyncSTM action =
    unforkAsync Unfork{ threadSafeAction, step }
  where
    threadSafeAction run arg = do
        resultVar <- newTVar Nothing
        enqueue run Task{ arg, resultVar }
        pure (readTVar resultVar)

    step Task{ arg, resultVar } = do
        b <- action arg
        atomically (writeTVar resultVar (Just b))


{- ━━━━━  I/O, asynchronous, with task results available  ━━━━━━━━━

    Similar to its STM counterpart, but uses MVar instead of TVar.

-}

unforkAsyncIO ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> IO (Future result)) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkAsyncIO action =
    unforkAsync Unfork{ threadSafeAction, step }
  where
    threadSafeAction run arg = do
        resultVar <- newEmptyMVar
        atomically (enqueue run Task{ arg, resultVar })
        pure (Future{ await = readMVar resultVar, peek = tryReadMVar resultVar })
    step Task{ arg, resultVar } = do
          b <- action arg
          putMVar resultVar b


{- ━━━━━  I/O, synchronous, with task results available  ━━━━━━━━━━

    Unlike the async unfork functions, the blocking version
    doesn't require us to spawn any threads. We just grab an
    MVar to ensure mutual exclusion while the action runs.

    Synchronous unforking can recover from exceptions thrown
    by the action (in contrast with the async functions, where
    an exception in the queue loop crashes both threads).

-}

unforkSyncIO ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> IO result) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkSyncIO action continue = do
    lock <- newMVar Lock
    continue \x -> do
        bracket (takeMVar lock) (putMVar lock) (\Lock -> action x)


{- ━━━━━  I/O, synchronous, with task results discarded  ━━━━━━━━━━

    Trivial; same as previous, but with the action voided first.

-}

unforkSyncIO_ ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> IO ()) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkSyncIO_ action =
    unforkSyncIO \x -> do
        _ <- action x
        pure ()


{- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ -}

data Unfork a c =
    forall q. Unfork
        { threadSafeAction :: !(Run q -> a -> c)
        , step :: !(q -> IO ())
        }

data Task a b = Task{ arg :: !a, resultVar :: !b }

data Run q = Run{ queue :: !(TQueue q), stopper :: !(TVar Bool) }

data Future result = Future{ await :: IO result, peek :: IO (Maybe result) }

data Lock = Lock

unforkAsync :: Unfork a c -> ((a -> c) -> IO b) -> IO b
unforkAsync Unfork{ threadSafeAction, step } continue = do
    queue <- newTQueueIO
    stopper <- newTVarIO False

    let
        run = Run{ queue, stopper }
        loop = do{ action <- atomically (act <|> done); action }
        act  = do{ x <- readTQueue queue; pure do{ step x; loop } }
        done = do{ stop <- readTVar stopper; guard stop; pure (pure ()) }

    ((), c) <- concurrently loop (continue (threadSafeAction run))
    pure c

enqueue :: Run q -> q -> STM ()
enqueue Run{ queue } = writeTQueue queue
