{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.MQ.Protocol.Internal.Instances
  (
  ) where

import           Control.Monad                     ((>=>))
import           Data.ByteString                   as BS (ByteString)
import           Data.Map.Strict                   (Map, fromList, member, (!))
import           Data.MessagePack.Types.Class      (MessagePack (..))
import           Data.MessagePack.Types.Object     (Object)
import           System.MQ.Protocol.Internal.Types (Dictionary (..), Hash,
                                                    Message (..), Timestamp)

infix .=
(.=) :: (Ord a, MessagePack b) => a -> b -> (a, Object)
a .= b = (a, toObject b)

infix .!
(.!) :: (Monad m, MessagePack b) => Map ByteString Object -> ByteString -> m b
dict .! key | key `member` dict = fromObject $ dict ! key
            | otherwise = error $ ".! :: key " ++ show key ++ " is not an element of the dictionary."
instance Dictionary Message where
  toDictionary ConfigMessage{..} = fromList [ "id"         .= msgId
                                            , "pid"        .= msgPid
                                            , "creator"    .= msgCreator
                                            , "created_at" .= msgCreatedAt
                                            , "expires_at" .= msgExpiresAt
                                            , "spec"       .= msgSpec
                                            , "config"     .= msgConfig
                                            , "encoding"   .= msgEncoding
                                            ]
  toDictionary ResultMessage{..} = fromList [ "id"         .= msgId
                                            , "pid"        .= msgPid
                                            , "creator"    .= msgCreator
                                            , "created_at" .= msgCreatedAt
                                            , "expires_at" .= msgExpiresAt
                                            , "spec"       .= msgSpec
                                            , "result"     .= msgResult
                                            , "encoding"   .= msgEncoding
                                            ]
  toDictionary ErrorMessage{..} = fromList [ "id"         .= msgId
                                           , "pid"        .= msgPid
                                           , "creator"    .= msgCreator
                                           , "created_at" .= msgCreatedAt
                                           , "expires_at" .= msgExpiresAt
                                           , "spec"       .= msgSpec
                                           , "error"      .= msgError
                                           , "encoding"   .= msgEncoding
                                           ]
  toDictionary DataMessage{..} = fromList [ "id"         .= msgId
                                          , "pid"        .= msgPid
                                          , "creator"    .= msgCreator
                                          , "created_at" .= msgCreatedAt
                                          , "expires_at" .= msgExpiresAt
                                          , "spec"       .= msgSpec
                                          , "data"       .= msgData
                                          , "encoding"   .= msgEncoding
                                          ]
  fromDictionary dict = do
    (mId :: Hash)             <- dict .! "id"
    (mPid :: Hash)            <- dict .! "pid"
    (mCreator :: ByteString)  <- dict .! "creator"
    (mCreatedAt :: Timestamp) <- dict .! "created_at"
    (mExpiresAt :: Timestamp) <- dict .! "expires_at"
    (mSpec :: ByteString)     <- dict .! "spec"
    (mEncoding :: ByteString) <- dict .! "encoding"
    let result | "config" `member` dict = do
                  (mConfig :: ByteString) <- dict .! "config"
                  pure ConfigMessage { msgId        = mId
                                     , msgPid       = mPid
                                     , msgCreator   = mCreator
                                     , msgCreatedAt = mCreatedAt
                                     , msgExpiresAt = mExpiresAt
                                     , msgSpec      = mSpec
                                     , msgConfig    = mConfig
                                     , msgEncoding  = mEncoding
                                     }
               | "result" `member` dict = do
                  (mResult :: ByteString) <- dict .! "result"
                  pure ResultMessage { msgId        = mId
                                     , msgPid       = mPid
                                     , msgCreator   = mCreator
                                     , msgCreatedAt = mCreatedAt
                                     , msgExpiresAt = mExpiresAt
                                     , msgSpec      = mSpec
                                     , msgResult    = mResult
                                     , msgEncoding  = mEncoding
                                     }
               | "error" `member` dict = do
                  (mError :: ByteString) <- dict .! "error"
                  pure ErrorMessage { msgId        = mId
                                    , msgPid       = mPid
                                    , msgCreator   = mCreator
                                    , msgCreatedAt = mCreatedAt
                                    , msgExpiresAt = mExpiresAt
                                    , msgSpec      = mSpec
                                    , msgError     = mError
                                    , msgEncoding  = mEncoding
                                    }
               | "data" `member` dict = do
                  (mData :: ByteString) <- dict .! "data"
                  pure DataMessage { msgId        = mId
                                   , msgPid       = mPid
                                   , msgCreator   = mCreator
                                   , msgCreatedAt = mCreatedAt
                                   , msgExpiresAt = mExpiresAt
                                   , msgSpec      = mSpec
                                   , msgData      = mData
                                   , msgEncoding  = mEncoding
                                   }
               | otherwise =  error $ "fromDictionary :: unknown constructor type"
    result

instance MessagePack Message where
  toObject = toObject . toDictionary
  fromObject = fromObject >=> fromDictionary

