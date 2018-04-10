{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Scheduler.Config
  (
    SchedulerConfig (..)
  , getSchedulerConfig
  ) where

import           Data.Aeson.Picker   ((|--))
import           System.BCD.Config   (getConfigText)
import           System.MQ.Transport (Host, Port)


data SchedulerConfig = SchedulerConfig { hostScheduler :: Host
                                       , portFromWorld :: Port
                                       , portToWorld   :: Port
                                       , portToLogic   :: Port
                                       , portFromLogic :: Port
                                       }

getSchedulerConfig :: IO SchedulerConfig
getSchedulerConfig = do
    config <- getConfigText
    let getField field = config |-- ["deploy", "monique", field]
    pure $ SchedulerConfig (getField "host-scheduler")
                           (getField "port-from-world")
                           (getField "port-to-world")
                           (getField "port-to-logic")
                           (getField "port-from-logic")
