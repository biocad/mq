{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.MQ.Transport.Internal.Instances () where

import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Data.Text                          as T (intercalate, pack,
                                                          snoc)
import           Data.Text.Encoding                 (encodeUtf8)
import           System.MQ.Transport.Internal.Types
import           System.ZMQ4                        (Pub (..), Pull (..),
                                                     Push (..), Socket,
                                                     SocketType, Sub (..), bind,
                                                     connect, socket, subscribe,
                                                     unsubscribe)

instance ConnectTo PushChannel where
  connectTo HostPort{..} context' = createAndConnect context' Push host port

instance ConnectTo PullChannel where
  connectTo HostPort{..} context' = createAndConnect context' Pull host port

instance ConnectTo PubChannel where
  connectTo HostPort{..} context' = createAndConnect context' Pub host port

instance ConnectTo SubChannel where
  connectTo HostPort{..} context' = createAndConnect context' Sub host port

instance BindTo PushChannel where
  bindTo HostPort{..} context' = createAndBind context' Push host port

instance BindTo PullChannel where
  bindTo HostPort{..} context' = createAndBind context' Pull host port

instance BindTo PubChannel where
  bindTo HostPort{..} context' = createAndBind context' Pub host port

instance BindTo SubChannel where
  bindTo HostPort{..} context' = createAndBind context' Sub host port

instance Subscribe SubChannel where
  subscribeTo socket' topic = liftIO . subscribe socket' $ encodeUtf8 topic

  subscribeToTypeSpec socket' type' spec' =
      let topic = T.intercalate ":" [T.pack . show $ type', spec'] `T.snoc` ':'
      in subscribeTo socket' topic

  unsubscribeFrom socket' topic = liftIO . unsubscribe socket' $ encodeUtf8 topic

  unsubscribeFromTypeSpec socket' type' spec' =
      let topic = T.intercalate ":" [T.pack . show $ type', spec'] `T.snoc` ':'
      in unsubscribeFrom socket' topic

--------------------------------------------------
-- INTERNAL
--------------------------------------------------

-- | Creates 'Socket' and for given 'Context', 'SocketType', 'Host' and 'Port'.
-- This type of sockets is used when destination address is unknown.
--
createAndBind :: (MonadIO m, SocketType a) => Context -> a -> Host -> Port -> m (Socket a)
createAndBind = createAndAction bind

-- | Creates 'Socket' and for given 'Context', 'SocketType', 'Host' and 'Port'.
-- This type of sockets is used when destination address is known.
--
createAndConnect :: (MonadIO m, SocketType a) => Context -> a -> Host -> Port -> m (Socket a)
createAndConnect = createAndAction connect

-- | Inner function which 'connect's or 'bind's to Socket.
--
createAndAction :: (MonadIO m, SocketType a) => (Socket a -> String -> IO ()) -> Context -> a -> Host -> Port -> m (Socket a)
createAndAction action context' socketType host port  = do
    socket' <- liftIO $ socket context' socketType
    liftIO $ action socket' (showTCP host port)
    pure socket'

