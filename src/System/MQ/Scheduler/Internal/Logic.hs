{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.Logic
  (
    schedulerLogic
  ) where

import           Control.Monad                       (forever)
import           Control.Monad.Except                (catchError, liftIO)
import           System.Log.Logger                   (infoM)
import           System.MQ.Monad                     (MQMonad, errorHandler)
import           System.MQ.Scheduler.Internal.Config (LogicConfig (..),
                                                      NetConfig (..))
import           System.MQ.Transport                 (ConnectTo (..),
                                                      PullChannel, PushChannel,
                                                      contextM)
import           System.MQ.Transport.ByteString      (pull, push)

schedulerLogic :: NetConfig -> LogicConfig -> MQMonad ()
schedulerLogic NetConfig{..} _ = do
    connect <- connections
    liftIO . infoM name $ "start working..."
    forever . (`catchError` errorHandler name) $ processing connect
  where
    connections :: MQMonad (PullChannel, PushChannel)
    connections = do
        context' <- contextM
        fromIn   <- connectTo schedulerInInner context'
        toOut    <- connectTo schedulerOutInner context'
        return (fromIn, toOut)

    name :: String
    name = "SchedulerLogic"

    processing :: (PullChannel, PushChannel) -> MQMonad ()
    processing (fromIn, toOut) = pull fromIn >>= (`push` toOut)

{-
  -- | Name of the commponent for the logging.
  name :: a -> String

  -- | Running function for the component.
  run :: a -> SchedulerConfig -> MQMonad ()
  run a cfg = do
    let name' = name a
    connect <- liftIO $ connections a cfg
    liftIO . infoM name' $ "start working..."
    forever . (`catchError` errorHandler name') $ processing a connect

  -- | Function that defines component behavior
  processing :: a -> Connections a -> MQMonad ()



type Connections SchedulerLogic = (PullChannel, PushChannel)

  name _ = "SchedulerLogic"

  connections _ SchedulerConfig{..} = do
      context' <- context
      fromIn   <- connectTo schedulerInInner context'
      toOut    <- connectTo schedulerOutInner context'
      return (fromIn, toOut)

  -- | For this moment there is no logic :).
  processing _ (fromIn, toOut) = pull fromIn >>= (`push` toOut)

-}
