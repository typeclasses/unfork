{-

Unlike the async unfork functions, the blocking version doesn't require us to spawn any threads. We just grab an MVar to ensure mutual exclusion while the action runs.

Synchronous unforking can recover from exceptions thrown by the action (in contrast with the async functions, where an exception in the queue loop crashes both threads).

-}

module Unfork.Sync (unforkSyncIO, unforkSyncIO_) where

import Prelude (IO, pure)

import Control.Exception.Safe (bracket)

import qualified Control.Concurrent.MVar as MVar

{- |

    Unforks an action by blocking on a mutex lock

    Related functions:

      - Use 'unforkSyncIO_' if you don't need the action's result
      - Consider instead using 'Unfork.unforkAsyncIO', which uses a queue and a separate thread, to avoid blocking

-}

unforkSyncIO ::
    (task -> IO result) -- ^ Action that needs to be run serially
    -> IO (task -> IO result) -- ^ The unforked action

unforkSyncIO action = do
    lock <- MVar.newMVar ()
    pure \x ->
        bracket (MVar.takeMVar lock) (MVar.putMVar lock) \() ->
            action x

{- |

    Unforks an action by blocking on a mutex lock, discarding the action's result

    Related functions:

      - Use 'unforkSyncIO' if you need the action's result
      - Consider instead using 'Unfork.unforkAsyncIO_', which uses a queue and a separate thread, to avoid blocking

-}

unforkSyncIO_ ::
    (task -> IO result) -- ^ Action that needs to be run serially
    -> IO (task -> IO ()) -- ^ The unforked action

unforkSyncIO_ action =
    unforkSyncIO \x -> do
        _ <- action x
        pure ()
