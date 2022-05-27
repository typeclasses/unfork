module Unfork where

import Prelude (IO, Bool (..), Maybe (..), pure)

import Control.Applicative ((<|>))

import Control.Exception.Safe (bracket)

import Control.Monad (guard)

import Control.Monad.STM (STM, atomically, retry)

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
unforkAsyncVoidSTM ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> STM ()) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkAsyncVoidSTM action =
    unforkAsync Unfork{ threadSafeAction, step }
  where
    threadSafeAction run arg = enqueue run arg
    step a = do{ _ <- action a; pure () }


{- ━━━━━  I/O, asynchronous, with task results discarded  ━━━━━━━━━

    This is just the same as its STM equivalent, but with an
    'atomically' thrown in for convenience.

-}

-- | Turns an IO action into a fire-and-forget async action
unforkAsyncVoidIO ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> IO ()) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkAsyncVoidIO action =
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
    (a -> IO b)
        -- ^ Action that needs to be run serially
    -> ((a -> STM (STM b)) -> IO c)
        -- ^ Continuation with a thread-safe version of the action
    -> IO c
unforkAsyncSTM action =
    unforkAsync Unfork{ threadSafeAction, step }
  where
    threadSafeAction run arg = do
        resultVar <- newTVar Nothing
        enqueue run Task{ arg, resultVar }
        pure do
            m <- readTVar resultVar
            case m of
                Nothing -> retry
                Just x -> pure x

    step Task{ arg, resultVar } = do
        b <- action arg
        atomically (writeTVar resultVar (Just b))


{- ━━━━━  I/O, asynchronous, with task results available  ━━━━━━━━━

    Similar to its STM counterpart, but uses MVar instead of TVar.

-}

unforkAsyncIO ::
    (a -> IO b)
        -- ^ Action that needs to be run serially
    -> ((a -> IO (Promise b)) -> IO c)
        -- ^ Continuation with a thread-safe version of the action
    -> IO c
unforkAsyncIO action =
    unforkAsync Unfork{ threadSafeAction, step }
  where
    threadSafeAction run arg = do
        resultVar <- newEmptyMVar
        atomically (enqueue run Task{ arg, resultVar })
        pure (Promise{ block = readMVar resultVar, peek = tryReadMVar resultVar })
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
    (a -> IO b)
        -- ^ Action that needs to be run serially
    -> ((a -> IO b) -> IO c)
        -- ^ Continuation with a thread-safe version of the action
    -> IO c
unforkSyncIO action continue = do
    lock <- newMVar Lock
    continue \x -> do
        bracket (takeMVar lock) (putMVar lock) (\Lock -> action x)


{- ━━━━━  I/O, synchronous, with task results discarded  ━━━━━━━━━━

    Trivial; same as previous, but with the action voided first.

-}

unforkSyncVoidIO ::
    (a -> IO b)
        -- ^ Action that needs to be run serially
    -> ((a -> IO ()) -> IO c)
        -- ^ Continuation with a thread-safe version of the action
    -> IO c
unforkSyncVoidIO action =
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

data Promise a = Promise{ block :: IO a, peek :: IO (Maybe a) }

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
