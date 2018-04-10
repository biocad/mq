module Main where

import           System.MQ.Scheduler (getSchedulerConfig, SchedulerIn (..), Scheduler (..))

main :: IO ()
main = do
    config <- getSchedulerConfig
    run SchedulerIn config
