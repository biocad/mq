module Main where

import           System.MQ.Monad     (runMQMonad)
import           System.MQ.Scheduler (Scheduler (..), SchedulerOut (..),
                                      getSchedulerConfig)

main :: IO ()
main = do
    config <- getSchedulerConfig
    runMQMonad $ run SchedulerOut config
