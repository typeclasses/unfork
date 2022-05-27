module Demo where

import Prelude (IO, String, putStr, show, (*>), (<>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Data.Foldable (for_)
import Numeric.Natural (Natural)

putStrLnSlow :: String -> IO ()
putStrLnSlow = r
  where
    r [] = putStr "\n"
    r (x : xs) = putStr [x] *> threadDelay 10 *> r xs

logFromTwoThreads :: (String -> IO ()) -> IO ()
logFromTwoThreads log =
    concurrently_
        (for_ [1 .. 9 :: Natural] (\i -> log ("A " <> show i <> "\tone two three four") *> threadDelay 50))
        (for_ [1 .. 9 :: Natural] (\i -> log ("B " <> show i <> "\tfive six seven eight") *> threadDelay 200))
