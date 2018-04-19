{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.In
  (
    schedulerIn
  ) where

import           Control.Monad                       (forever)
import           Control.Monad.Except                (catchError, liftIO)
import           System.Log.Logger                   (infoM)
import           System.MQ.Monad                     (MQMonad, errorHandler)
import           System.MQ.Scheduler.Internal.Config (InConfig, NetConfig (..))
import           System.MQ.Transport                 (BindTo (..),
                                                      HostPort (..),
                                                      PullChannel, PushChannel,
                                                      anyHost, contextM)
import           System.MQ.Transport.ByteString      (pull, push)

schedulerIn :: NetConfig -> InConfig -> MQMonad ()
schedulerIn NetConfig{..} _ = do
    connect <- connections
    liftIO . infoM name $ "start working..."
    forever . (`catchError` errorHandler name) $ processing connect
  where
    connections :: MQMonad (PullChannel, PushChannel)
    connections = do
        context'  <- contextM
        fromWorld <- bindTo fromWorldHP context'
        toLogic   <- bindTo toLogicHP context'
        return (fromWorld, toLogic)

    name :: String
    name = "SchedulerIn"

    fromWorldHP = HostPort anyHost (port schedulerInOuter)
    toLogicHP   = HostPort anyHost (port schedulerInInner)

    processing :: (PullChannel, PushChannel) -> MQMonad ()
    processing (fromWorld, toLogic) = pull fromWorld >>= push toLogic
