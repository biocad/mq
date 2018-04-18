{-# LANGUAGE ViewPatterns #-}

module System.MQ.Encoding.JSON
  (
    pack
  , unpack
  , unpackM
  ) where

import           Control.Monad.Except (throwError)
import           Data.Aeson           (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import           System.MQ.Monad      (MQError (..), MQMonad)
import           Text.Printf          (printf)

-- | Pack something from JSON to 'BS.ByteString'.
--
pack :: ToJSON a => a -> BS.ByteString
pack = BSL.toStrict . encode

-- | Unpack something from 'BS.ByteString' to JSON.
-- If 'unpack' failes then 'Nothing' will be returned.
--
unpack :: FromJSON a => BS.ByteString -> Maybe a
unpack = decode . BSL.fromStrict

-- | Unpack something from 'BS.ByteString' to JSON inside 'MQMonad'.
-- If 'unpackM' failes then 'MQError' will be thrown.
--
unpackM :: FromJSON a => BS.ByteString -> MQMonad a
unpackM bs@(unpack -> m)  = maybe (throwError err) pure m
  where
    err = MQProtocolError . printf "could not unpack JSON: %s" . show $ bs
