module Main (main) where

import Demo
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    logFromTwoThreads putStrLnSlow
