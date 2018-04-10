module Main where

import           System.MQ.Scheduler (getSchedulerConfig, runSchedulerLogic)

main :: IO ()
main = do
    config <- getSchedulerConfig
    runSchedulerLogic config
