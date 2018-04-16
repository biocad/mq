{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Protocol.Internal.Types
  (
    Timestamp
  , Hash
  , MessageTag
  , Message (..)
  , Dictionary (..)
  , createConfigMessage
  , createResultMessage
  , createErrorMessage
  , createDataMessage
  , jsonEncoding
  , msgpackEncoding
  ) where

import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Crypto.Hash.SHA1              (hash)
import           Data.ByteString               as BS (ByteString, intercalate)
import           Data.Map.Strict               (Map)
import           Data.MessagePack              ()
import           Data.MessagePack.Types.Object (Object)
import           Data.String                   (IsString (..))
import           GHC.Generics                  (Generic (..))
import           System.Clock                  (Clock (..), getTime, toNanoSecs)

-- | Dictionary class describes objects that can be turned into an association list (key := value)
-- Object
class Dictionary a where
  toDictionary :: a -> Map ByteString Object
  fromDictionary :: Monad m => Map ByteString Object -> m a

jsonEncoding :: ByteString
jsonEncoding = "JSON"

msgpackEncoding :: ByteString
msgpackEncoding = "MessagePack"

-- | Represents Unix epoch time in milliseconds.
--
type Timestamp  = Int

-- | Represents SHA-1 hash sum.
--
type Hash       = ByteString

-- | Message tag is a bytestring with five separated with ':' fields: message type; spec; id; pid; creator.
-- See doc/PROTOCOL.md for more details.
--
type MessageTag = ByteString

-- | Message is the main entity in MQ: various components, controllers and the Scheduler communicate with each other using Messages.
--
data Message
  = ConfigMessage { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: ByteString
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: ByteString
                  , msgConfig    :: ByteString
                  , msgEncoding  :: ByteString
                  }
  | ResultMessage { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: ByteString
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: ByteString
                  , msgResult    :: ByteString
                  , msgEncoding  :: ByteString
                  }
  | ErrorMessage  { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: ByteString
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: ByteString
                  , msgError     :: ByteString
                  , msgEncoding  :: ByteString
                  }
  | DataMessage   { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: ByteString
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: ByteString
                  , msgData      :: ByteString
                  , msgEncoding  :: ByteString
                  }
  deriving (Eq, Show, Read, Generic)


createConfigMessage :: MonadIO m => Hash
                                 -> ByteString
                                 -> Timestamp
                                 -> ByteString
                                 -> ByteString
                                 -> ByteString
                                 -> m Message
createConfigMessage mPid mCreator mExpires mSpec mConfig mEncoding = do
  mCreated <- getTimeMilli
  let mId = hash $ intercalate ":" [mCreator, timestampToBS mCreated, mSpec]

  pure $ ConfigMessage mId mPid mCreator mCreated mExpires mSpec mConfig mEncoding

createResultMessage :: MonadIO m => Hash
                                 -> ByteString
                                 -> Timestamp
                                 -> ByteString
                                 -> ByteString
                                 -> ByteString
                                 -> m Message
createResultMessage mPid mCreator mExpires mSpec mResult mEncoding = do
  mCreated <- getTimeMilli
  let mId = hash $ intercalate ":" [mCreator, timestampToBS mCreated, mSpec]

  pure $ ResultMessage mId mPid mCreator mCreated mExpires mSpec mResult mEncoding

createErrorMessage :: MonadIO m => Hash
                                -> ByteString
                                -> Timestamp
                                -> ByteString
                                -> ByteString
                                -> ByteString
                                -> m Message
createErrorMessage mPid mCreator mExpires mSpec mError mEncoding = do
  mCreated <- getTimeMilli
  let mId = hash $ intercalate ":" [mCreator, timestampToBS mCreated, mSpec]

  pure $ ErrorMessage mId mPid mCreator mCreated mExpires mSpec mError mEncoding

createDataMessage :: MonadIO m => Hash
                               -> ByteString
                               -> Timestamp
                               -> ByteString
                               -> ByteString
                               -> ByteString
                               -> m Message
createDataMessage mPid mCreator mExpires mSpec mData mEncoding = do
  mCreated <- getTimeMilli
  let mId = hash $ intercalate ":" [mCreator, timestampToBS mCreated, mSpec]

  pure $ DataMessage mId mPid mCreator mCreated mExpires mSpec mData mEncoding

getTimeMilli :: MonadIO m => m Timestamp
getTimeMilli = (`div` 10^(6::Int)) <$> getTimeNano

getTimeNano :: MonadIO m => m Timestamp
getTimeNano = liftIO $ fromIntegral . toNanoSecs <$> getTime Realtime

timestampToBS :: Timestamp -> ByteString
timestampToBS = fromString . show
