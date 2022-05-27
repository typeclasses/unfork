{- |

“Unfork” is the opposite of “fork”; whereas forking
allows things to run concurrently, unforking prevents
things from running concurrently.

+-----------+------------------+-------------------+
|           | Result available | Result discarded  |
+-----------+------------------+-------------------+
| Async I/O | 'unforkAsyncIO'  | 'unforkAsyncIO_'  |
+-----------+------------------+-------------------+
| Async STM | 'unforkAsyncSTM' | 'unforkAsyncSTM_' |
+-----------+------------------+-------------------+
| Sync I/O  | 'unforkSyncIO'   | 'unforkSyncIO_'   |
+-----------+------------------+-------------------+

Use one of the functions in this module when you
have an action that will be used by concurrent
threads but needs to run serially.


== Example

A typical use case is a multi-threaded program that writes log
messages. If threads use 'System.IO.putStrLn' directly, the strings
may be interleaved in the combined output.

@'Control.Concurrent.Async.concurrently_' ('System.IO.putStrLn' "one") ('System.IO.putStrLn' "two")@

Instead, create an unforked version of 'System.IO.putStrLn'.

@'unforkAsyncIO_' 'System.IO.putStrLn' $ \\log ->
    'Control.Concurrent.Async.concurrently_' (log "one") (log "two")@


== Asynchrony

The four async functions are 'unforkAsyncIO', 'unforkAsyncIO_',
'unforkAsyncSTM', 'unforkAsyncSTM_'.

> unforkAsyncIO   :: (a -> IO b) -> ( ( a -> IO (Future b)       ) -> IO c ) -> IO c
> unforkAsyncIO_  :: (a -> IO b) -> ( ( a -> IO ()               ) -> IO c ) -> IO c
> unforkAsyncSTM  :: (a -> IO b) -> ( ( a -> STM (STM (Maybe b)) ) -> IO c ) -> IO c
> unforkAsyncSTM_ :: (a -> IO b) -> ( ( a -> STM ()              ) -> IO c ) -> IO c
>                    |         |    | |                          |         |
>                    |---------|    | |--------------------------|         |
>                     Original      |      Unforked action                 |
>                      action       |                                      |
>                                   |--------------------------------------|
>                                               Continuation

These functions all internally use a queue.
The unforked action does not perform the underlying action at all,
but instead merely writes to the queue. A separate thread reads
from the queue and performs the actions, thus ensuring that the
actions are all performed in one linear sequence.

There are, therefore, three threads of concern to this library:

  1. the one running the user-provided continuation
  2. the one performing the enqueued actions
  3. the parent thread that owns the other two

Non-exceptional termination works as follows:

  - Thread 1 reaches its normal end and halts
  - Thread 2 finishes processing any remaining queued jobs,
    then halts
  - Thread 3 halts

Threads 1 and 2 are “linked”, in the parlance of
"Control.Concurrent.Async"; if either thread throws an exception,
then the other action is cancelled, and the exception is
re-thrown by thread 3. Likewise, any exception that is thrown to
the parent thread will result in the cancellation of it children.
In other words, if anything fails, then the entire system fails
immediately. This is desirable for two reasons:

  - It avoids the risk of leaving any dangling threads
  - No exceptions are “swallowed”; if something fails,
    you will see the exception.

If this is undesirable, you can change the behavior by catching
and handling exceptions. If you want a system that is resilient
to failures of the action, then unfork an action that catches
exceptions. If you want a system that finishes processing the
queue even after the continuation fails, then use a continuation
that catches and handles exceptions.


== Results

The functions in this module come in pairs: one that provides some
means of obtaining the result, and one (ending in an underscore)
that discards the action's result.

In the asynchronous case, the result-discarding functions provide no
means of even determining whether the action has completed yet;
we describe these as "fire-and-forget" functions, because there
is no further interaction the initiator of an action can have with
it after the action has begun.

The async functions that do provide results are 'unforkAsyncSTM' and
'unforkAsyncIO'. Internally, each result is stored in a
'Control.Concurrent.STM.TVar' or 'Control.Concurrent.MVar.MVar',
respectively. These variables are exposed to the user in a read-only
way:

  - 'unforkAsyncSTM' gives access to its 'Control.Concurrent.STM.TVar'
    via @('STM' ('Maybe' result))@, whose value is 'Nothing' while the
    action is in flight, and 'Just' thereafter.
  - 'unforkAsyncIO' gives access to its 'Control.Concurrent.MVar.MVar'
    via @('Future' result)@. The 'Future' type offers two functions:
    'poll' to see the current status ('Nothing' while the action is in
    flight, and 'Just' thereafter), and 'await' to block until the
    action completes.

In both cases, an action is either pending or successful. There is no
representation of a “threw an exception” action result. This is
because of the “if anything fails, then the entire system fails
immediately” property discussed in the previous section. If an action
throws an exception, your continuation won't live long enough to
witness it anyway because it will be immediately killed.


== Synchrony

The two sync functions are 'unforkSyncIO' and 'unforkSyncIO_'.

> unforkSyncIO  :: (a -> IO b) -> IO (a -> IO b )
> unforkSyncIO_ :: (a -> IO b) -> IO (a -> IO ())
>                  |         |       |          |
>                  |---------|       |----------|
>                Original action    Unforked action

These are much simpler than their asynchronous counterparts; there
is no queue, no new threads are spawned, and therefore no
continuation-passing is needed. These simply produce a variant of
the action that is 'bracket'ed by acquisition and release of an
'Control.Concurrent.MVar.MVar' to assure mutual exclusion.

The hazard of the synchronous approach is that the locking has a
greater potential to bottleneck performance.

-}
module Unfork
  (
    {- * Asynchronous I/O -} unforkAsyncIO_, unforkAsyncIO,
                             Future, await, poll,
    {- * Asynchronous STM -} unforkAsyncSTM_, unforkAsyncSTM,
    {- * Synchronous I/O  -} unforkSyncIO_, unforkSyncIO,
  ) where

import Prelude (IO, Maybe (..), Eq ((==)), pure)

import Control.Applicative ((<|>))
import Control.Exception.Safe (bracket)
import Control.Monad (guard, join)
import Control.Monad.STM (STM, atomically)
import Control.Concurrent.Async (concurrently)
import Data.Functor (($>), (<&>))

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM


{- ━━━━━  STM, asynchronous, with task results discarded  ━━━━━━━━━

    Discarding results makes this function simpler than those that
    make results available. All we do is maintain a queue of tasks,
    and each step of the queue loop runs the action.

-}

{- | Turns an IO action into a fire-and-forget STM action

    Related functions:

      - 'unforkAsyncSTM' does not discard the action result, and
        it allows polling or waiting for completion
      - 'unforkAsyncIO_' gives the unforked action result as 'IO'
        instead of 'STM'
-}
unforkAsyncSTM_ ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> STM ()) -> IO conclusion)
        -- ^ Continuation with the unforked action
    -> IO conclusion
unforkAsyncSTM_ action =
    unforkAsync Unfork{ unforkedAction, executeOneTask }
  where
    unforkedAction run arg = enqueue run arg
    executeOneTask a = do{ _ <- action a; pure () }


{- ━━━━━  I/O, asynchronous, with task results discarded  ━━━━━━━━━

    This is just the same as its STM equivalent, but with an
    'atomically' thrown in for convenience.

-}

{- | Turns an IO action into a fire-and-forget async action

    For example, use @('unforkAsyncIO_' 'System.IO.putStrLn')@
    to log to 'System.IO.stdout' in a multi-threaded application.

    Related functions:

      - 'unforkAsyncIO' does not discard the action result, and
        it allows polling or waiting for completion
      - 'unforkAsyncSTM_' gives the unforked action result as
        'STM' instead of 'IO'
-}
unforkAsyncIO_ ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> IO ()) -> IO conclusion)
        -- ^ Continuation with the unforked action
    -> IO conclusion
unforkAsyncIO_ action =
    unforkAsync Unfork{ unforkedAction, executeOneTask }
  where
    unforkedAction run arg = atomically (enqueue run arg)
    executeOneTask a = do{ _ <- action a; pure () }


{- ━━━━━  STM, asynchronous, with task results available  ━━━━━━━━━

    To make task results available, we maintain a queue that
    contains not only each the task itself, but also a TVar to
    store its result. Each step of the queue loop runs the action
    and then places the result into the TVar.

-}

{- | Unforks an action, with the new action's asynchronous result
     available as @('STM' ('Maybe' result))@

    Related functions:

      - Use 'unforkAsyncSTM_' if you do not need to know when the
        action has completed or obtain its result value
      - Use 'unforkAsyncIO' if you do not need the composability
        of 'STM'

-}
unforkAsyncSTM ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> STM (STM (Maybe result))) -> IO conclusion)
        -- ^ Continuation with the unforked action
    -> IO conclusion
unforkAsyncSTM action =
    unforkAsync Unfork{ unforkedAction, executeOneTask }
  where
    unforkedAction run arg = do
        resultVar <- STM.newTVar Nothing
        enqueue run Task{ arg, resultVar }
        pure (STM.readTVar resultVar)

    executeOneTask Task{ arg, resultVar } = do
        b <- action arg
        atomically (STM.writeTVar resultVar (Just b))


{- ━━━━━  I/O, asynchronous, with task results available  ━━━━━━━━━

    Similar to its STM counterpart, but uses MVar instead of TVar.

-}

{- | Unforks an action, with the new action's asynchronous result
     available as @('IO' ('Future' result))@

    Related functions:

      - Use 'unforkAsyncIO_' if you do not need to know when the
        action has completed or obtain its result value
      - Use 'unforkAsyncSTM' if you need the composability of 'STM'

-}
unforkAsyncIO ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> ((task -> IO (Future result)) -> IO conclusion)
        -- ^ Continuation with the unforked action
    -> IO conclusion
unforkAsyncIO action =
    unforkAsync Unfork{ unforkedAction, executeOneTask }
  where
    unforkedAction run arg = do
        resultVar <- MVar.newEmptyMVar
        atomically (enqueue run Task{ arg, resultVar })
        pure (Future resultVar)
    executeOneTask Task{ arg, resultVar } = do
          b <- action arg
          MVar.putMVar resultVar b


{- ━━━━━  I/O, synchronous, with task results available  ━━━━━━━━━━

    Unlike the async unfork functions, the blocking version
    doesn't require us to spawn any threads. We just grab an
    MVar to ensure mutual exclusion while the action runs.

    Synchronous unforking can recover from exceptions thrown
    by the action (in contrast with the async functions, where
    an exception in the queue loop crashes both threads).

-}

{- | Unforks an action by blocking on a global lock.

    Related functions:

      - Use 'unforkSyncIO_' if you don't need the action's result
      - Consider instead using 'unforkAsyncIO', which uses a queue
        and a separate thread, to avoid blocking
-}
unforkSyncIO ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> IO (task -> IO result)
        -- ^ The unforked action
unforkSyncIO action = do
    lock <- MVar.newMVar Lock
    pure \x ->
        bracket (MVar.takeMVar lock) (MVar.putMVar lock) \Lock ->
            action x


{- ━━━━━  I/O, synchronous, with task results discarded  ━━━━━━━━━━

    Trivial; same as previous, but with the action voided first.

-}

{- | Unforks an action by blocking on a global lock.

    Related functions:

      - Use 'unforkSyncIO' if you need the action's result
      - Consider instead using 'unforkAsyncIO_', which uses a
        queue and a separate thread, to avoid blocking
-}
unforkSyncIO_ ::
    (task -> IO result)
        -- ^ Action that needs to be run serially
    -> IO (task -> IO ())
        -- ^ The unforked action
unforkSyncIO_ action =
    unforkSyncIO \x -> do
        _ <- action x
        pure ()


{- ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ -}

data Unfork a c =
    forall q. Unfork
        { unforkedAction :: !(Run q -> a -> c)
            -- ^ The unforked version of the action that we
            --   give to the user-provided continuation
        , executeOneTask :: !(q -> IO ())
            -- ^ How the queue worker processes each item
        }

data Task a b = Task{ arg :: !a, resultVar :: !b }

data Run q =
    Run{ queue :: !(STM.TQueue q), stopper :: !(STM.TVar Status) }

data Status = Stop | Go
    deriving Eq

{- | The result of an action unforked by 'unforkAsyncIO'

    At first the result will be unavailable, during which time
    'await' will block and 'poll' will return 'Nothing'. When
    the action completes, 'await' will return its result and
    'poll' will return 'Just'.
-}
data Future result = Future (MVar.MVar result)

-- | Block until an action completes
await :: Future result -> IO result
await (Future v) = MVar.readMVar v

-- | Returns 'Just' an action's result, or 'Nothing' if the
-- action is not yet complete
poll :: Future result -> IO (Maybe result)
poll (Future v) = MVar.tryReadMVar v

data Lock = Lock

unforkAsync :: Unfork a c -> ((a -> c) -> IO b) -> IO b
unforkAsync Unfork{ unforkedAction, executeOneTask } continue =
  do
    run <- do
        queue <- STM.newTQueueIO
        stopper <- STM.newTVarIO Go
        pure Run{ queue, stopper }

    let
        loop = join (atomically (act <|> done))
        act = next run <&> \x -> do{ executeOneTask x; loop }
        done = checkStopped run $> pure ()

    ((), c) <- concurrently loop do
        x <- continue (unforkedAction run)
        stop run
        pure x

    pure c

enqueue :: Run q -> q -> STM ()
enqueue Run{ queue } = STM.writeTQueue queue

next :: Run q -> STM q
next Run{ queue } = STM.readTQueue queue

checkStopped :: Run q -> STM ()
checkStopped Run{ stopper } = do
    s <- STM.readTVar stopper
    guard (s == Stop)

stop :: Run q -> IO ()
stop Run{ stopper } =
    atomically (STM.writeTVar stopper Stop)
