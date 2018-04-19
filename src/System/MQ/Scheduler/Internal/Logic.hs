{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.Logic
  (
    schedulerLogic
  ) where

import           Control.Monad                       (forever)
import           Control.Monad.Except                (catchError, liftIO)
import           Data.ByteString                     (ByteString)
import           Data.String                         (IsString (..))
import           System.Log.Logger                   (infoM)
import           System.MQ.Monad                     (MQMonad, errorHandler)
import           System.MQ.Protocol                  (messageSpec)
import           System.MQ.Scheduler.Internal.Config (LogicConfig (..),
                                                      NetConfig (..))
import           System.MQ.Transport                 (ConnectTo (..),
                                                      PullChannel, PushChannel,
                                                      contextM)
import           System.MQ.Transport.ByteString      (pull, push)

-- | Scheduler logic receives messages from scheduler in, make some logic and (maybe) sends it to scheduler out.
-- For this moment following logic is in it:
--
schedulerLogic :: NetConfig -> LogicConfig -> MQMonad ()
schedulerLogic NetConfig{..} LogicConfig{..} = do
    connect <- connections
    liftIO . infoM name $ "start working..."
    forever . (`catchError` errorHandler name) $ processing allowList connect
  where
    connections :: MQMonad (PullChannel, PushChannel)
    connections = do
        context' <- contextM
        fromIn   <- connectTo schedulerInInner context'
        toOut    <- connectTo schedulerOutInner context'
        return (fromIn, toOut)

    allowList :: [ByteString]
    allowList = fromString <$> allowMessages

    name :: String
    name = "SchedulerLogic"

    processing :: [ByteString] -> (PullChannel, PushChannel) -> MQMonad ()
    -- if list of allow messages is empty then send every message further
    processing [] (fromIn, toOut) = pull fromIn >>= push toOut
    -- else only messages with spec from @allowList@ are send further
    processing allowList' (fromIn, toOut) = do
        m@(tag, _) <- pull fromIn
        if messageSpec tag `elem` allowList'
        then push toOut m
        else pure ()
