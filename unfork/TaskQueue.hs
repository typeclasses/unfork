module TaskQueue (Task (..), doTask, enqueueTask) where

import Prelude (IO, Maybe (..), maybe)
import Control.Concurrent.STM (STM, TVar)

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad as Monad

data Task a b = Task{ arg :: a, resultVar :: TVar (Maybe b) }

doTask :: (a -> IO b) -> Task a b -> IO ()
doTask action Task{ arg, resultVar } =
  do
    b <- action arg
    STM.atomically (STM.writeTVar resultVar (Just b))

enqueueTask :: forall a b. STM.TQueue (Task a b) -> a -> STM (STM b)
enqueueTask queue arg =
  do
    resultVar <- STM.newTVar @(Maybe b) Nothing
    STM.writeTQueue queue Task{ arg, resultVar }
    Monad.return do
        bMaybe <- STM.readTVar resultVar
        maybe STM.retry Monad.return bMaybe
