module Main where

import           System.MQ.Scheduler (getConfig, runSchedulerIn)

main :: IO ()
main = do
    config <- getConfig
    runSchedulerIn config
