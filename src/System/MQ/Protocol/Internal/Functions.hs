{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Protocol.Internal.Functions
  (
    emptyHash
  , notExpires
  , isConfig
  , jsonEncoding
  , msgpackEncoding
  , createConfigMessage
  , createResultMessage
  , createErrorMessage
  , createDataMessage
  , getTimeMillis
  ) where

import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Crypto.Hash.SHA1                  (hash)
import           Data.ByteString                   as BS (ByteString,
                                                          intercalate)
import           Data.String                       (IsString (..))
import           System.Clock                      (Clock (..), getTime,
                                                    toNanoSecs)
import           System.MQ.Protocol.Internal.Types (Creator, Encoding, Hash,
                                                    Message (..), Spec,
                                                    Timestamp)

-- | Creates 'Hash' with empty content.
--
emptyHash :: Hash
emptyHash = ""

-- | If message has no expiration time then this function can be used.
--
notExpires :: Timestamp
notExpires = 0

-- | Alias for JSON encoding.
--
jsonEncoding :: Encoding
jsonEncoding = "JSON"

-- | Alias for MessagePack encoding.
--
msgpackEncoding :: Encoding
msgpackEncoding = "MessagePack"


createConfigMessage :: MonadIO m => Hash
                                 -> Creator
                                 -> Timestamp
                                 -> Spec
                                 -> Encoding
                                 -> ByteString
                                 -> m Message
createConfigMessage mPid mCreator mExpires mSpec mEncoding mConfig = do
    (mId, mCreated) <- mkId mCreator mSpec
    pure $ ConfigMessage mId mPid mCreator mCreated mExpires mSpec mEncoding mConfig

createResultMessage :: MonadIO m => Hash
                                 -> Creator
                                 -> Timestamp
                                 -> Spec
                                 -> Encoding
                                 -> ByteString
                                 -> m Message
createResultMessage mPid mCreator mExpires mSpec mEncoding mResult = do
    (mId, mCreated) <- mkId mCreator mSpec
    pure $ ResultMessage mId mPid mCreator mCreated mExpires mSpec mEncoding mResult

createErrorMessage :: MonadIO m => Hash
                                -> Creator
                                -> Timestamp
                                -> Spec
                                -> Encoding
                                -> ByteString
                                -> m Message
createErrorMessage mPid mCreator mExpires mSpec mEncoding mError = do
    (mId, mCreated) <- mkId mCreator mSpec
    pure $ ErrorMessage mId mPid mCreator mCreated mExpires mSpec mEncoding mError

createDataMessage :: MonadIO m => Hash
                               -> Creator
                               -> Timestamp
                               -> Spec
                               -> Encoding
                               -> ByteString
                               -> m Message
createDataMessage mPid mCreator mExpires mSpec mEncoding mData = do
    (mId, mCreated) <- mkId mCreator mSpec
    pure $ DataMessage mId mPid mCreator mCreated mExpires mSpec mEncoding mData

-- | Get current time in milliseconds.
--
getTimeMillis :: MonadIO m => m Timestamp
getTimeMillis = (`div` 10^(6::Int)) <$> getTimeNano

--------------------------------------------------------------------------------
-- INTERNAL
--------------------------------------------------------------------------------

-- | Creates id 'Hash' and created time from 'Creator' and 'Spec'.
--
mkId :: MonadIO m => Creator -> Spec -> m (Hash, Timestamp)
mkId mCreator mSpec = do
    mCreated <- getTimeMillis
    let mId = hash $ intercalate ":" [fromString mCreator, timestampToBS mCreated, fromString mSpec]
    pure (mId, mCreated)

getTimeNano :: MonadIO m => m Timestamp
getTimeNano = liftIO $ fromIntegral . toNanoSecs <$> getTime Realtime

timestampToBS :: Timestamp -> ByteString
timestampToBS = fromString . show

isConfig :: Message -> Bool
isConfig ConfigMessage{} = True
isConfig _               = False
