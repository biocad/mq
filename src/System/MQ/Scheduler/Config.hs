{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Scheduler.Config
  (
    SchedulerConfig (..)
  , getSchedulerConfig
  ) where

import           Data.Aeson.Picker   ((|--))
import           System.BCD.Config   (getConfigText)
import           System.MQ.Transport (HostPort (..))

-- | 'SchedulerConfig' contains all information for schedulers connections.
--
data SchedulerConfig = SchedulerConfig { schedulerInOuter  :: HostPort
                                       , schedulerInInner  :: HostPort
                                       , schedulerOutOuter :: HostPort
                                       , schedulerOutInner :: HostPort
                                       }

-- | Function to get schedulers config from config.json.
--
getSchedulerConfig :: IO SchedulerConfig
getSchedulerConfig = do
    config           <- getConfigText
    let getIn field  = config |-- ["deploy", "monique", "scheduler-in", field]
    let getOut field = config |-- ["deploy", "monique", "scheduler-out", field]
    pure $ SchedulerConfig (HostPort (getIn "host") (getIn "port-outer"))
                           (HostPort (getIn "host") (getIn "port-inner"))
                           (HostPort (getOut "host") (getOut "port-outer"))
                           (HostPort (getOut "host") (getOut "port-inner"))
