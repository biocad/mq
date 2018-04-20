{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.Out
  (
    runSchedulerOut
  ) where

import           Control.Monad                       (forever)
import Control.Concurrent (forkIO)
import           Control.Monad.Except                (catchError, liftIO)
import           System.Log.Logger                   (infoM)
import           System.MQ.Monad                     (MQMonad, errorHandler, runMQMonad)
import           System.MQ.Scheduler.Internal.Config (NetConfig (..), OutConfig, SchedulerCfg, comHostPort, techHostPort)
import           System.MQ.Transport                 (BindTo (..),
                                                      HostPort (..), PubChannel,
                                                      PullChannel, anyHost,
                                                      contextM)
import           System.MQ.Transport.ByteString      (pub, pull)

runSchedulerOut :: NetConfig -> IO ()
runSchedulerOut NetConfig{..} = do
    -- connect <- connections
    liftIO . infoM name $ "start working..."
    _ <- forkIO $ processing techHostPort
    processing comHostPort
  where
    -- connections :: MQMonad (PullChannel, PubChannel)
    -- connections = do
    --     context'  <- contextM
    --     fromLogic <- bindTo fromLogicHP context'
    --     toWorld   <- bindTo toWorldHP context'
    --     return (fromLogic, toWorld)

    -- fromLogicHP = HostPort anyHost (port schedulerOutInner)
    -- toWorldHP   = HostPort anyHost (port schedulerOutOuter)

    name :: String
    name = "SchedulerOut"

    -- processing :: (PullChannel, PubChannel) -> MQMonad ()
    -- processing (fromLogic, toWorld) = pull fromLogic >>= pub toWorld

    processing :: (SchedulerCfg -> HostPort) -> IO ()
    processing hostPortSelector = runMQMonad $ do
        context' <- contextM
        let fromLogicHP = HostPort anyHost (port . hostPortSelector $ schedulerLogicOut)
        fromLogic <- bindTo fromLogicHP context'
        toWorld <- bindTo (hostPortSelector schedulerOut) context'
        forever . (`catchError` errorHandler name) $ (pull fromLogic >>= pub toWorld)
