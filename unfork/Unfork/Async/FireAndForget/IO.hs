{-

I/O, asynchronous, with task results discarded

This is just the same as its STM equivalent, but with an 'atomically' thrown in for convenience.

-}

module Unfork.Async.FireAndForget.IO
    (
        unforkAsyncIO_,
    )
    where

import Unfork.Async.Core

import Prelude (IO, pure)

import Control.Monad.STM (atomically)

{- |

    Turns an IO action into a fire-and-forget async action

    For example, use @('unforkAsyncIO_' 'System.IO.putStrLn')@ to log to 'System.IO.stdout' in a multi-threaded application.

    Related functions:

      - 'Unfork.unforkAsyncIO' does not discard the action result, and it allows polling or waiting for completion
      - 'Unfork.unforkAsyncSTM_' gives the unforked action result as 'Control.Monad.STM.STM' instead of 'IO'

-}

unforkAsyncIO_ ::
    (task -> IO result) -- ^ Action that needs to be run serially
    -> ((task -> IO ()) -> IO conclusion) -- ^ Continuation with the unforked action
    -> IO conclusion

unforkAsyncIO_ action =
    unforkAsync Unfork{ unforkedAction, executeOneTask }
  where
    unforkedAction ctx arg = atomically (enqueue ctx arg)
    executeOneTask a = do{ _ <- action a; pure () }
