module Main where

import           System.MQ.Scheduler (getConfig, runSchedulerOut)

main :: IO ()
main = do
    config <- getConfig
    runSchedulerOut config
