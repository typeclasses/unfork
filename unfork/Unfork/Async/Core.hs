{-

    This module constitutes the main contribution of the library

-}

module Unfork.Async.Core where

import Prelude (Eq ((==)), IO, pure)

import Control.Applicative ((<|>))
import Control.Concurrent.Async (concurrently)
import Control.Monad (guard, join)
import Control.Monad.STM (STM, atomically)
import Data.Functor (($>), (<&>))

import qualified Control.Concurrent.STM as STM

data Unfork a c = forall q. Unfork
  { unforkedAction ::         --   The unforked action that we give
      !( Ctx q -> a -> c )    --  to the user-provided continuation
  , executeOneTask ::
      !( q -> IO () )   -- How the queue worker processes each item
  }

data Ctx q = Ctx          -- Mutable context for an async unforking
  { queue :: !(STM.TQueue q)
  , stopper :: !(STM.TVar Status)
  }

data Status = Stop | Go          deriving Eq

enqueue :: Ctx q -> q -> STM ()              --  Write to the queue
enqueue Ctx{ queue } = STM.writeTQueue queue

next :: Ctx q -> STM q                      --  Read from the queue
next Ctx{ queue } = STM.readTQueue queue

stop :: Ctx q -> IO ()   --  Indicate to the queue loop thread that
stop Ctx{ stopper } =    --  it should stop once all tasks are done
    atomically (STM.writeTVar stopper Stop)

checkStopped :: Ctx q -> STM ()     --     STM action that succeeds
checkStopped Ctx{ stopper } = do    --  only if 'stop' has been run
    s <- STM.readTVar stopper
    guard (s == Stop)

unforkAsync ::                   --        This is the basis of all
    Unfork a c                   --  four async unforking functions
    -> ((a -> c) -> IO b)
    -> IO b
unforkAsync Unfork{ unforkedAction, executeOneTask } continue =
  do
    ctx <- do
        queue <- STM.newTQueueIO
        stopper <- STM.newTVarIO Go
        pure Ctx{ queue, stopper }

    let
      loop = join (atomically (act <|> done))
        where
          act = next ctx <&> \x -> do{ executeOneTask x; loop }
          done = checkStopped ctx $> pure ()

    ((), c) <- concurrently loop do
        x <- continue (unforkedAction ctx)
        stop ctx
        pure x

    pure c
