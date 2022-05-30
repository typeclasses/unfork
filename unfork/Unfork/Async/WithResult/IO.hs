{-

I/O, asynchronous, with task results available

Similar to its STM counterpart, but uses MVar instead of TVar.

-}

module Unfork.Async.WithResult.IO
    (
        unforkAsyncIO,
        Future, await, poll,
    )
    where

import Unfork.Async.Core
import Unfork.Async.WithResult.Future
import Unfork.Async.WithResult.Task

import Prelude (IO, pure)

import Control.Monad.STM (atomically)

import qualified Control.Concurrent.MVar as MVar

{- |

    Unforks an action, with the new action's asynchronous result available as @('IO' ('Future' result))@

    Related functions:

      - Use 'Unfork.unforkAsyncIO_' if you do not need to know when the action has completed or obtain its result value
      - Use 'Unfork.unforkAsyncSTM' if you need the composability of 'Control.Monad.STM.STM'

-}

unforkAsyncIO ::
    (task -> IO result) -- ^ Action that needs to be run serially
    -> ((task -> IO (Future result)) -> IO conclusion) -- ^ Continuation with the unforked action
    -> IO conclusion

unforkAsyncIO action =
    unforkAsync Unfork{ unforkedAction, executeOneTask }
  where
    unforkedAction ctx arg = do
        resultVar <- MVar.newEmptyMVar
        atomically (enqueue ctx Task{ arg, resultVar })
        pure (Future resultVar)
    executeOneTask Task{ arg, resultVar } = do
          b <- action arg
          MVar.putMVar resultVar b
