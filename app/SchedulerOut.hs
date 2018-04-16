module Main where

import           System.MQ.Monad     (runMQMonad)
import           System.MQ.Scheduler (getNetConfig, getOutConfig, schedulerOut)

main :: IO ()
main = do
    netConfig <- getNetConfig
    outConfig <- getOutConfig
    runMQMonad $ schedulerOut netConfig outConfig
