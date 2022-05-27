# Unfork

“Unfork” is the opposite of “fork”; whereas forking allows things
to run concurrently, unforking prevents things from running concurrently.

|             | Result available   | Result discarded   |
| ----------- | ------------------ | ------------------ |
| Async I/O   | `unforkAsyncIO`    | `unforkAsyncIO_`   |
| Async STM   | `unforkAsyncSTM`   | `unforkAsyncSTM_`  |
| Sync I/O    | `unforkSyncIO`     | `unforkSyncIO_`    |

Use one of the functions in the `Unfork` module when you have an action that
will be used by concurrent threads but needs to run serially.

## Example

A typical use case is a multi-threaded program that writes log messages. If
threads use `putStrLn` directly, the strings may be interleaved in the combined
output.

```haskell
concurrently_ (putStrLn "one") (putStrLn "two")
```

Instead, create an unforked version of `putStrLn`.

```haskell
unforkAsyncIO_ putStrLn $ \log ->
    concurrently_ (log "one") (log "two")
```


## Asynchrony

The four async functions are `unforkAsyncIO`, `unforkAsyncIO_`, `unforkAsyncSTM`,
and `unforkAsyncSTM_`.

```haskell
unforkAsyncIO   :: (a -> IO b) -> ( ( a -> IO (Future b)       ) -> IO c ) -> IO c
unforkAsyncIO_  :: (a -> IO b) -> ( ( a -> IO ()               ) -> IO c ) -> IO c
unforkAsyncSTM  :: (a -> IO b) -> ( ( a -> STM (STM (Maybe b)) ) -> IO c ) -> IO c
unforkAsyncSTM_ :: (a -> IO b) -> ( ( a -> STM ()              ) -> IO c ) -> IO c
--                 |         |    | |                          |         |
--                 |---------|    | |--------------------------|         |
--                  Original      |      Unforked action                 |
--                   action       |                                      |
--                                |--------------------------------------|
--                                            Continuation
```

These functions all internally use a queue. The unforked action does not perform
the underlying action at all, but instead merely writes to the queue. A separate
thread reads from the queue and performs the actions, thus ensuring that the
actions are all performed in one linear sequence.

There are, therefore, three threads of concern to this library:

  1. the one running the user-provided continuation
  2. the one performing the enqueued actions
  3. the parent thread that owns the other two

Non-exceptional termination works as follows:

  - Thread 1 concludes
  - Thread 2 finishes processing any remaining queued jobs, then returns
  - Thread 3 returns

Threads 1 and 2 are “linked”, in the parlance of the `async` package; if either
thread throws an exception, then the other action is cancelled, and the
exception is re-thrown by thread 3. Likewise, any exception that is thrown to
the parent thread will result in the cancellation of it children. In other
words, if anything fails, then the entire system fails immediately. This is
desirable for two reasons:

  - It avoids the risk of leaving any dangling threads
  - No exceptions are “swallowed”; if something fails, you will see the
    exception.

If this is undesirable, you can change the behavior by catching and handling
exceptions. If you want a system that is resilient to failures of the action,
then unfork an action that catches exceptions. If you want a system that
finishes processing the queue even after the continuation fails, then use a
continuation that catches and handles exceptions.


## Results

The functions in this module come in pairs: one that provides some means of
obtaining the result, and one (ending in an underscore) that discards the
action's result.

In the asynchronous case, the result-discarding functions provide no means of
even determining whether the action has completed yet; we describe these as
"fire-and-forget" functions, because there is no further interaction the
initiator of an action can have with it after the action has begun.

The async functions that do provide results are `unforkAsyncSTM` and
`unforkAsyncIO`. Internally, each result is stored in a `TVar` or `MVar`,
respectively. These variables are exposed to the user in a read-only way:

  - `unforkAsyncSTM` gives access to its `TVar` via `STM (Maybe result)`,
    whose value is `Nothing` while the action is in flight, and `Just`
    thereafter.

  - `unforkAsyncIO` gives access to its `MVar` via `Future' result`.
    The `Future` type offers two functions: `poll` to see the current
    status (`Nothing` while the action is in flight, and `Just` thereafter),
    and `await` to block until the action completes.


## Synchrony

The two sync functions are `unforkSyncIO` and `unforkSyncIO_`.

```haskell
unforkSyncIO  :: (a -> IO b) -> IO (a -> IO b )
unforkSyncIO_ :: (a -> IO b) -> IO (a -> IO ())
--               |         |       |          |
--               |---------|       |----------|
--             Original action    Unforked action
```

These are much simpler than their asynchronous counterparts; there is no queue,
no new threads are spawned, and therefore no continuation-passing is needed.
These simply produce a variant of the action that is `bracket`ed by acquisition
and release of an `MVar` to assure mutual exclusion.

The hazard of the synchronous approach is that the locking has a greater
potential to bottleneck performance.
