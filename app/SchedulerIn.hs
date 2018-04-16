module Main where

import           System.MQ.Monad     (runMQMonad)
import           System.MQ.Scheduler (getInConfig, getNetConfig, schedulerIn)

main :: IO ()
main = do
    netConfig <- getNetConfig
    inConfig  <- getInConfig
    runMQMonad $ schedulerIn netConfig inConfig
