module Main where

import           System.MQ.Scheduler (getSchedulerConfig, SchedulerOut (..), Scheduler (..))

main :: IO ()
main = do
    config <- getSchedulerConfig
    run SchedulerOut config
