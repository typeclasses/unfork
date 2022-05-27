module Unfork where

import Prelude (IO, Bool (..), Maybe (..), pure)

import Control.Applicative ((<|>))

import Control.Monad (guard)

import Control.Monad.STM (STM, atomically, retry)

import Control.Concurrent.Async (concurrently)

import Control.Concurrent.MVar
    (newEmptyMVar, putMVar, readMVar)

import Control.Concurrent.STM.TQueue
    (TQueue, newTQueueIO, readTQueue, writeTQueue)

import Control.Concurrent.STM.TVar
    (TVar, newTVar, newTVarIO, readTVar, writeTVar)


{- ━━━━━━━━━━━━  I/O, with task results discarded  ━━━━━━━━━━━━━━━━

    This is just the same as its STM equivalent, but with an
    'atomically' thrown in for convenience.

-}

unforkAsyncVoidIO ::
    (task -> IO result)
        -- ^ Action that needs to be run from a single thread
    -> ((task -> IO ()) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkAsyncVoidIO action go =
    unforkAsyncVoidSTM action \action' ->
        go \x -> atomically (action' x)


{- ━━━━━━━━━━━━  STM, with task results discarded  ━━━━━━━━━━━━━━━━

    Discarding results makes this function simpler than those that
    make results available. All we do is maintain a queue of tasks,
    and each step of the queue loop runs the action.

-}

unforkAsyncVoidSTM ::
    (task -> IO result)
        -- ^ Action that needs to be run from a single thread
    -> ((task -> STM ()) -> IO conclusion)
        -- ^ Continuation with a thread-safe version of the action
    -> IO conclusion
unforkAsyncVoidSTM action = unfork Unfork{ threadSafeAction, step }
  where
    threadSafeAction = enqueue
    step a = do{ _ <- action a; pure () }


{- ━━━━━━━━━━━━  STM, with task results available  ━━━━━━━━━━━━━━━━

    To make task results available, we maintain a queue that
    contains not only each the task itself, but also a TVar to
    store its result. Each step of the queue loop runs the action
    and then places the result into the TVar.

-}

unforkAsyncSTM ::
    (a -> IO b)
        -- ^ Action that needs to be run from a single thread
    -> ((a -> STM (STM b)) -> IO c)
        -- ^ Continuation with a thread-safe version of the action
    -> IO c
unforkAsyncSTM action = unfork Unfork{ threadSafeAction, step }
  where
    threadSafeAction run arg = do
        resultVar <- newTVar Nothing
        enqueue run Task{ arg, resultVar }
        pure (readTVarJust resultVar)

    step Task{ arg, resultVar } = do
        b <- action arg
        atomically (writeTVar resultVar (Just b))


{- ━━━━━━━━━━━━  I/O, with task results available  ━━━━━━━━━━━━━━━━

    Similar to its STM counterpart, but uses MVar instead of TVar.

-}

unforkAsyncIO ::
    (a -> IO b)
        -- ^ Action that needs to be run from a single thread
    -> ((a -> IO (IO b)) -> IO c)
        -- ^ Continuation with a thread-safe version of the action
    -> IO c
unforkAsyncIO action = unfork Unfork{ threadSafeAction, step }
  where
    threadSafeAction run arg = do
        resultVar <- newEmptyMVar
        atomically (enqueue run Task{ arg, resultVar })
        pure (readMVar resultVar)
    step Task{ arg, resultVar } = do
          b <- action arg
          putMVar resultVar b


{- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ -}

unfork :: Unfork a c -> ((a -> c) -> IO b) -> IO b
unfork Unfork{ threadSafeAction, step } continue = do
    run <- newRun
    (c, ()) <- concurrently (continue (threadSafeAction run))
                            (queueLoop run step)
    pure c

data Unfork a c =
    forall q. Unfork
        { threadSafeAction :: !(Run q -> a -> c)
        , step :: !(q -> IO ())
        }

data Task a b = Task{ arg :: !a, resultVar :: !b }

data Run q = Run{ queue :: !(TQueue q), stopper :: !(TVar Bool) }

newRun :: IO (Run q)
newRun = do
    queue <- newTQueueIO
    stopper <- newTVarIO False
    pure Run{ queue, stopper }

enqueue :: Run q -> q -> STM ()
enqueue Run{ queue } = writeTQueue queue

queueLoop :: Run q -> (q -> IO ()) -> IO ()
queueLoop Run{ queue, stopper } go = loop
  where
    loop     = do action <- atomically (continue <|> finish)
                  action

    continue = do x <- readTQueue queue
                  pure do{ go x; loop }

    finish   = do stop <- readTVar stopper
                  guard stop
                  pure (pure ())

readTVarJust :: TVar (Maybe a) -> STM a
readTVarJust v = do
    m <- readTVar v
    case m of
        Nothing -> retry
        Just x -> pure x
