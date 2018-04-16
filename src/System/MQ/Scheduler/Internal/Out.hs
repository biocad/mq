{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.Out
  (
    schedulerOut
  ) where

import           Control.Monad                       (forever)
import           Control.Monad.Except                (catchError, liftIO)
import           System.Log.Logger                   (infoM)
import           System.MQ.Monad                     (MQMonad, errorHandler)
import           System.MQ.Scheduler.Internal.Config (NetConfig (..), OutConfig)
import           System.MQ.Transport                 (BindTo (..),
                                                      HostPort (..), PubChannel,
                                                      PullChannel, anyHost,
                                                      contextM)
import           System.MQ.Transport.ByteString      (pub, pull)

schedulerOut :: NetConfig -> OutConfig -> MQMonad ()
schedulerOut NetConfig{..} _ = do
    connect <- connections
    liftIO . infoM name $ "start working..."
    forever . (`catchError` errorHandler name) $ processing connect
  where
    connections :: MQMonad (PullChannel, PubChannel)
    connections = do
        context'  <- contextM
        fromLogic <- bindTo fromLogicHP context'
        toWorld   <- bindTo toWorldHP context'
        return (fromLogic, toWorld)

    fromLogicHP = HostPort anyHost (port schedulerOutInner)
    toWorldHP   = HostPort anyHost (port schedulerOutOuter)

    name :: String
    name = "SchedulerOut"

    processing :: (PullChannel, PubChannel) -> MQMonad ()
    processing (fromLogic, toWorld) = pull fromLogic >>= (`pub` toWorld)
