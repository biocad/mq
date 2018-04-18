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
import           System.MQ.Protocol.Internal.Types (Hash, Timestamp)

-- | Configuration for kill task
--
data KillConfig = KillConfig { killTaskId :: Hash
                             } deriving (Eq, Show, Generic)

instance ToJSON KillConfig where
  toJSON p = object [ "task_id" .= (BSLC8.unpack $ killTaskId p) ]
instance FromJSON KillConfig where
  parseJSON = withObject "Kill Config" $ \o -> KillConfig . BSLC8.pack <$> o .: "task_id"

-- | Configuration of monitoring task
--
data MonitoringConfig = MonitoringConfig { syncTime :: Timestamp
                                         } deriving (Eq, Show, Generic)

instance ToJSON MonitoringConfig where
  toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON MonitoringConfig where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

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
