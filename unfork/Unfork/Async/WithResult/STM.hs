{-

STM, asynchronous, with task results available

To make task results available, we maintain a queue that contains not only each the task itself, but also a TVar to store its result. Each step of the queue loop runs the action and then places the result into the TVar.

-}

module Unfork.Async.WithResult.STM
    (
        unforkAsyncSTM,
    )
    where

import Unfork.Async.Core
import Unfork.Async.WithResult.Task

import Prelude (IO, Maybe (..), pure)

import Control.Monad.STM (STM, atomically)

import qualified Control.Concurrent.STM as STM

{- |

    Unforks an action, with the new action's asynchronous result available as @('STM' ('Maybe' result))@

    Related functions:

      - Use 'Unfork.unforkAsyncSTM_' if you do not need to know when the action has completed or obtain its result value
      - Use 'Unfork.unforkAsyncIO' if you do not need the composability of 'STM'

-}

unforkAsyncSTM ::
    (task -> IO result) -- ^ Action that needs to be run serially
    -> ((task -> STM (STM (Maybe result))) -> IO conclusion) -- ^ Continuation with the unforked action
    -> IO conclusion

unforkAsyncSTM action =
    unforkAsync Unfork{ unforkedAction, executeOneTask }
  where
    unforkedAction ctx arg = do
        resultVar <- STM.newTVar Nothing
        enqueue ctx Task{ arg, resultVar }
        pure (STM.readTVar resultVar)

    executeOneTask Task{ arg, resultVar } = do
        b <- action arg
        atomically (STM.writeTVar resultVar (Just b))
