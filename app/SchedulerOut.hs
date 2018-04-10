module Main where

import           System.MQ.Scheduler (getSchedulerConfig, runSchedulerOut)

main :: IO ()
main = do
    config <- getSchedulerConfig
    runSchedulerOut config
