{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Scheduler.Internal.Config
  (
    NetConfig (..)
  , InConfig (..)
  , LogicConfig (..)
  , OutConfig (..)
  , getNetConfig
  , getLogicConfig
  , getInConfig
  , getOutConfig
  ) where

import           Data.Aeson.Picker   ((|--))
import           System.BCD.Config   (getConfigText)
import           System.MQ.Transport (HostPort (..))

-- | Contains all network information for schedulers.
--
data NetConfig = NetConfig { schedulerInOuter  :: HostPort
                           , schedulerInInner  :: HostPort
                           , schedulerOutOuter :: HostPort
                           , schedulerOutInner :: HostPort
                           }

-- | Function to get net config for schedulers from config.json.
--
getNetConfig :: IO NetConfig
getNetConfig = do
    config           <- getConfigText
    let getIn field  = config |-- ["deploy", "monique", "scheduler-in", field]
    let getOut field = config |-- ["deploy", "monique", "scheduler-out", field]
    pure $ NetConfig (HostPort (getIn "host") (getIn "port-outer"))
                     (HostPort (getIn "host") (getIn "port-inner"))
                     (HostPort (getOut "host") (getOut "port-outer"))
                     (HostPort (getOut "host") (getOut "port-inner"))



-- | Contains scheduler in configuration.
--
data InConfig = InConfig {}

-- | Contains scheduler logic configuration.
--
newtype LogicConfig = LogicConfig { allowMessages :: [String] }

-- | Contains scheduler out configuration.
--
data OutConfig = OutConfig {}

-- | Load scheduler in configuration from config.json.
--
getInConfig :: IO InConfig
getInConfig = pure InConfig

-- | Load scheduler logic configuration from config.json.
--
getLogicConfig :: IO LogicConfig
getLogicConfig = do
    config             <- getConfigText
    let getField field = config |-- ["params", "scheduler-logic", field]
    pure $ LogicConfig (getField "allow-messages")

-- | Load scheduler out configuration from config.json.
--
getOutConfig :: IO OutConfig
getOutConfig = pure OutConfig
