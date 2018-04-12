module Main where

import           System.MQ.Monad     (runMQMonad)
import           System.MQ.Scheduler (Scheduler (..), SchedulerIn (..),
                                      getSchedulerConfig)

main :: IO ()
main = do
    config <- getSchedulerConfig
    runMQMonad $ run SchedulerIn config
