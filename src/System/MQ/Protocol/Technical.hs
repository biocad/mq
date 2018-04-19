{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.MQ.Protocol.Technical
  ( KillConfig (..)
  , MonitoringConfig (..)
  , MonitoringResult (..)
  ) where

import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    genericParseJSON,
                                                    genericToJSON, object,
                                                    withObject, (.:), (.=))
import           Data.Aeson.Casing                 (aesonPrefix, snakeCase)
import qualified Data.ByteString.Char8             as BSLC8 (pack, unpack)
import           GHC.Generics                      (Generic)
import           System.MQ.Protocol (Hash, Timestamp, MessageType (..), jsonEncoding)
import System.MQ.Encoding.JSON as JSON (pack, unpack)
import System.MQ.Protocol.Class

-- | Configuration for kill task
--
newtype KillConfig = KillConfig { killTaskId :: Hash }
  deriving (Eq, Show, Generic)

instance ToJSON KillConfig where
  toJSON p = object [ "task_id" .= BSLC8.unpack (killTaskId p) ]

instance FromJSON KillConfig where
  parseJSON = withObject "Kill Config" $ \o -> KillConfig . BSLC8.pack <$> o .: "task_id"

instance MessageLike KillConfig where
  props = Props "kill" Config jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack



-- | Configuration of monitoring task
--
newtype MonitoringConfig = MonitoringConfig { syncTime :: Timestamp }
  deriving (Eq, Show, Generic)

instance ToJSON MonitoringConfig where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON MonitoringConfig where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance MessageLike MonitoringConfig where
  props = Props "monitoring" Config jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack

-- | Format of data that is produced as result of monitoring task
--
data MonitoringResult = MonitoringResult { rSyncTime  :: Timestamp
                                         , rName      :: String
                                         , rHost      :: String
                                         , rIsRunning :: Bool
                                         , rMessage   :: String
                                         } deriving (Eq, Show, Generic)

instance ToJSON MonitoringResult where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON MonitoringResult where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance MessageLike MonitoringResult where
  props = Props "monitoring" Result jsonEncoding
  pack = JSON.pack
  unpack = JSON.unpack
