module Main where

import           System.MQ.Monad     (runMQMonad)
import           System.MQ.Scheduler (Scheduler (..), SchedulerLogic (..),
                                      getSchedulerConfig)

main :: IO ()
main = do
    config <- getSchedulerConfig
    runMQMonad $ run SchedulerLogic config
