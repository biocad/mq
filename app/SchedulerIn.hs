module Main where

import           System.MQ.Scheduler (getSchedulerConfig, runSchedulerIn)

main :: IO ()
main = do
    config <- getSchedulerConfig
    runSchedulerIn config
