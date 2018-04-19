{-# LANGUAGE ViewPatterns #-}

module System.MQ.Protocol.Class
  (
    MessageLike (..)
  , Props (..)
  ) where

import           Control.Monad.Except (throwError)
import           System.MQ.Monad      (MQError (..), MQMonad)
import           System.MQ.Protocol   (Encoding, MessageType, Spec)
import           Text.Printf          (printf)

import qualified Data.ByteString      as BS (ByteString)




-- | Every message in Monique system has fixed properties, that can be describe with type 'Props'.
--
data Props a = Props { spec     :: Spec        -- ^ message spec
                     , mtype    :: MessageType -- ^ message type (Config, Result, Error or Data)
                     , encoding :: Encoding    -- ^ message encoding
                     }

-- | 'MessageLike' connects meaningful information about message with it's properties.
-- "meaningful information" is 'ByteString' that is stored in one of the following message fields: config, result, error or data.
-- But content of the 'ByteString' is not enough to describe specification, type and encoding of the message.
-- So every message in haskell language should be instance of 'MessageLike'.
--
-- To make user happy with encoding of the messages standart functions should be definied.
-- It is possible to use standart functions from module 'System.MQ.Encoding'.
--
class MessageLike a where
  -- | Returns 'Props' for message.
  props :: Props a

  -- | Pack something to 'BS.ByteString'.
  pack :: a -> BS.ByteString

  -- | Unpack something from 'BS.ByteString'.
  -- If 'unpack' failes then 'Nothing' will be returned.
  --
  unpack :: BS.ByteString -> Maybe a

  -- | Unpack something from 'BS.ByteString' inside 'MQMonad'.
  -- If 'unpackM' failes then 'MQError' will be thrown.
  --
  unpackM :: BS.ByteString -> MQMonad a
  unpackM bs@(unpack -> m) = maybe (throwError err) pure m
    where
      err = MQProtocolError . printf "could not unpack JSON: %s" . show $ bs




