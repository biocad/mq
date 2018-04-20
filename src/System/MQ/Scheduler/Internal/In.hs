{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.In
  (
    runSchedulerIn
  ) where

import           Control.Concurrent                  (forkIO)
import           Control.Monad                       (forever)
import           Control.Monad.Except                (catchError, liftIO)
import           System.Log.Logger                   (infoM)
import           System.MQ.Monad                     (MQMonad, errorHandler,
                                                      runMQMonad)
import           System.MQ.Scheduler.Internal.Config (InConfig, NetConfig (..), comHostPort, techHostPort,
                                                      SchedulerCfg)
import           System.MQ.Transport                 (BindTo (..),
                                                      HostPort (..),
                                                      PullChannel, PushChannel,
                                                      anyHost, contextM)
import           System.MQ.Transport.ByteString      (pull, push)

runSchedulerIn :: NetConfig -> IO ()
runSchedulerIn NetConfig{..} = do
    -- connect <- connections
    -- liftIO . infoM name $ "start working..."
    -- forever . (`catchError` errorHandler name) $ processing connect
    infoM name "start working..."
    _ <- forkIO $ processing techHostPort
    processing comHostPort
  where
    -- connections :: MQMonad (PullChannel, PushChannel)
    -- connections = do
        -- context'  <- contextM
        -- fromWorld <- bindTo fromWorldHP context'
        -- toLogic   <- bindTo toLogicHP context'
        -- return (fromWorld, toLogic)

    name :: String
    name = "SchedulerIn"

    -- fromWorldHP = HostPort anyHost (port schedulerInOuter)
    -- toLogicHP   = HostPort anyHost (port schedulerInInner)

    -- processing :: (PullChannel, PushChannel) -> MQMonad ()
    -- processing (fromWorld, toLogic) = pull fromWorld >>= push toLogic

    processing :: (SchedulerCfg -> HostPort) -> IO ()
    processing hostPortSelector = runMQMonad $ do
        context' <- contextM
        let toLogicHP = HostPort anyHost (port . hostPortSelector $ schedulerInLogic)
        fromWorld <- bindTo (hostPortSelector schedulerIn) context'
        toLogic <- bindTo toLogicHP context'
        -- toWorkd <- bindTo (hostPortSelector schedulerOut) context'
        forever . (`catchError` errorHandler name) $ (pull fromWorld >>= push toLogic)
