{-

STM, asynchronous, with task results discarded

Discarding results makes this function simpler than those that make results available. All we do is maintain a queue of tasks, and each step of the queue loop runs the action.

-}

module Unfork.Async.FireAndForget.STM
    (
        unforkAsyncSTM_,
    )
    where

import Unfork.Async.Core

import Prelude (IO, pure)

import Control.Monad.STM (STM)

{- |

    Turns an IO action into a fire-and-forget STM action

    Related functions:

      - 'Unfork.unforkAsyncSTM' does not discard the action result, and it allows polling or waiting for completion
      - 'Unfork.unforkAsyncIO_' gives the unforked action result as 'IO' instead of 'STM'

-}

unforkAsyncSTM_ ::
    (task -> IO result) -- ^ Action that needs to be run serially
    -> ((task -> STM ()) -> IO conclusion) -- ^ Continuation with the unforked action
    -> IO conclusion

unforkAsyncSTM_ action =
    unforkAsync Unfork{ unforkedAction, executeOneTask }
  where
    unforkedAction ctx arg = enqueue ctx arg
    executeOneTask a = do{ _ <- action a; pure () }
