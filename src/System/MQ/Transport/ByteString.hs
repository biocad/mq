{-# LANGUAGE OverloadedStrings #-}


module System.MQ.Transport.ByteString
  (
    push
  , pull
  , pub
  , sub
  ) where

import           Control.Monad.Except
import qualified Data.ByteString                    as BS (ByteString)
import           Data.List.NonEmpty                 (NonEmpty (..))
import           System.MQ.Monad                    (MQError (..), MQMonad)
import           System.MQ.Transport.Internal.Types (PubChannel, PullChannel,
                                                     PushChannel, SubChannel)
import           System.ZMQ4                        (receiveMulti, sendMulti)
import           Text.Printf                        (printf)




-- | Pushes @(tag, content)@ to the 'PushChannel'.
--
push :: (BS.ByteString, BS.ByteString) -> PushChannel -> MQMonad ()
push (msgTag, msgContent) channel = liftIO . sendMulti channel $ msgTag :| [msgContent]

-- | Pulls @(tag, content)@ from the 'PullChannel'.
--
pull :: PullChannel -> MQMonad (BS.ByteString, BS.ByteString)
pull channel = do
    msg' <- liftIO . receiveMulti $ channel
    case msg' of
        [msgTag, msgContent] -> pure (msgTag, msgContent)
        _                    -> throwError . errorMsg $ length msg'

-- | Publishes @(tag, content)@ to the 'PubChannel'.
--
pub :: (BS.ByteString, BS.ByteString) -> PubChannel -> MQMonad ()
pub (msgTag, msgContent) channel = liftIO . sendMulti channel $ msgTag :| [msgContent]

-- | Subscribes and gets @(tag, content)@ from the 'SubChannel'.
--
sub :: SubChannel -> MQMonad (BS.ByteString, BS.ByteString)
sub channel = do
    msg' <- liftIO . receiveMulti $ channel
    case msg' of
        [msgTag, msgContent] -> pure (msgTag, msgContent)
        _                    -> throwError . errorMsg $ length msg'

errorMsg :: Int -> MQError
errorMsg = MQTransportError . printf "expected message with [header, body]; received list with %d element(s)."




