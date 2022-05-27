module RunUntilException (runUntilException, ShutdownBehavior (..)) where

import Prelude (IO, Maybe (..), (*>))
import Control.Concurrent.STM (TQueue)
import Control.Exception.Safe (finally, mask_)
import Control.Monad (forever, return)

import qualified Control.Concurrent.STM as STM
import qualified Data.Function as Function

data Run a b = Run{ queue :: TQueue a, action :: a -> IO b }

-- | What to do when it's time to stop but there are still items in the queue
data ShutdownBehavior =
    StopImmediately
      {- ^ Stop processing the queue, even if there are items left.
           If there is an item currently being processed, it may be
           interrupted by an async exception. -}
  | FinishQueue
      {- ^ Keep going until the queue is empty. This option ensures
           that every item that enters the queue will eventually get
           processed, at the risk of creating a process that does not
           respond to a kill signal promptly. -}

runUntilException :: forall a b. ShutdownBehavior -> TQueue a -> (a -> IO b) -> IO ()
runUntilException shutdown queue action =
    let x = Run{ queue, action } in
    case shutdown of
        FinishQueue      ->  forever (mask_ (doOne x)) `finally` runUntilEmpty x
        StopImmediately  ->  forever (doOne x)

doOne :: Run a b -> IO b
doOne Run{ action, queue } =
  do
    x <- STM.atomically (STM.readTQueue queue)
    action x

runUntilEmpty :: forall a b. Run a b -> IO ()
runUntilEmpty Run{ action, queue } =
    Function.fix \continue ->
      do
        xMay <- STM.atomically (STM.tryReadTQueue queue)
        case xMay of
            Nothing  ->  return ()
            Just x   ->  action x *> continue
