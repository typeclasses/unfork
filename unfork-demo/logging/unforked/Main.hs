module Main (main) where

import Demo
import System.IO
import Unfork

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    unforkAsyncIO_ putStrLnSlow logFromTwoThreads
