module Main where

import           System.MQ.Scheduler (getConfig, runSchedulerLogic)

main :: IO ()
main = do
    config <- getConfig
    runSchedulerLogic config
