{-# LANGUAGE RecordWildCards #-}

module System.MQ.Scheduler.Internal.Logic
  (
    runSchedulerLogic
  ) where

import           Control.Concurrent                  (forkIO)
import           Control.Monad                       (forever)
import           Control.Monad.Except                (catchError, liftIO)
import           Data.ByteString                     (ByteString)
import           Data.String                         (IsString (..))
import           System.Log.Logger                   (infoM)
import           System.MQ.Monad                     (MQMonad, errorHandler,
                                                      runMQMonad)
import           System.MQ.Protocol                  (messageSpec)
import           System.MQ.Scheduler.Internal.Config (LogicConfig (..),
                                                      NetConfig (..), OutConfig,
                                                      SchedulerCfg, comHostPort,
                                                      techHostPort)
import           System.MQ.Transport                 (ConnectTo (..),
                                                      PullChannel, PushChannel,
                                                      contextM)
import           System.MQ.Transport.ByteString      (pull, push)

-- | Scheduler logic receives messages from scheduler in, make some logic and (maybe) sends it to scheduler out.
-- For this moment following logic is in it:
--
runSchedulerLogic :: NetConfig -> LogicConfig -> IO ()
runSchedulerLogic NetConfig{..} LogicConfig{..} = do
    -- connect <- connections
    -- liftIO . infoM name $ "start working..."


    infoM name "start working..."
    _ <- forkIO processingTech
    processingCom allowList

    -- forever . (`catchError` errorHandler name) $ processing allowList connect
  where
    -- connections :: MQMonad (PullChannel, PushChannel)
    -- connections = do
    --     context' <- contextM
    --     fromIn   <- connectTo schedulerInInner context'
    --     toOut    <- connectTo schedulerOutInner context'
    --     return (fromIn, toOut)

    allowList :: [ByteString]
    allowList = fromString <$> allowMessages

    name :: String
    name = "SchedulerLogic"

    processingCom :: [ByteString] -> IO ()
    processingCom allowList' = runMQMonad $ do
        context' <- contextM
        fromInToLogic <- connectTo (comHostPort schedulerInLogic) context'
        fromLogictoWorld <- connectTo (comHostPort schedulerLogicOut) context'
        forever . (`catchError` errorHandler name) $ comLogic allowList' fromInToLogic fromLogictoWorld

    comLogic :: [ByteString] -> PullChannel -> PushChannel -> MQMonad ()
    -- if list of allow messages is empty then send every message further
    --
    comLogic [] fromIn toOut = pull fromIn >>= push toOut
    -- else only messages with spec from @allowList@ are send further
    --
    comLogic allowList' fromIn toOut = do
        m@(tag, _) <- pull fromIn
        if messageSpec tag `elem` allowList'
        then push toOut m
        else pure ()


    processingTech :: IO ()
    processingTech = runMQMonad $ do
        context' <- contextM
        fromInToLogic    <- connectTo (techHostPort schedulerInLogic) context'
        fromLogictoWorld <- connectTo (techHostPort schedulerLogicOut) context'
        forever . (`catchError` errorHandler name) $ (pull fromInToLogic >>= push fromLogictoWorld)


