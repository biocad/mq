module Main where

import           System.MQ.Scheduler (getSchedulerConfig, SchedulerLogic (..), Scheduler (..))

main :: IO ()
main = do
    config <- getSchedulerConfig
    run SchedulerLogic config
