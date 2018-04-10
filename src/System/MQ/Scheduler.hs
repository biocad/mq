{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Scheduler where

import           Control.Monad       (forever)
import           Data.List.NonEmpty  (NonEmpty (..))
import           System.MQ.Transport
import           System.ZMQ4         (Pub (..), Pull (..), Push (..), Socket,
                                      context, receiveMulti, sendMulti)
import           System.BCD.Config                (getConfigText)
import Data.Aeson.Picker ((|--))

{--
            World
              |
 < PULL | SchedulerIn | PUSH >
              |
< PULL | SchedulerLogic | PUSH > x n
              |
 < PULL | SchedulerOut | PUB >
              |
            World
--}


data SchedulerConfig = SchedulerConfig { hostScheduler :: Host
                                       , portFromWorld :: Port
                                       , portToWorld   :: Port
                                       , portToLogic   :: Port
                                       , portFromLogic :: Port
                                       }

getConfig :: IO SchedulerConfig
getConfig = do
    config <- getConfigText
    let getField field = config |-- ["deploy", "monique", field]
    pure $ SchedulerConfig (getField "host-scheduler")
                           (getField "port-from-world")
                           (getField "port-to-world")
                           (getField "port-to-logic")
                           (getField "port-from-logic")


runSchedulerIn :: SchedulerConfig -> IO ()
runSchedulerIn SchedulerConfig{..} = do
  (fromWorld, toLogic) <- connections
  forever $ do
     [msgHeader, msgBody] <- receiveMulti fromWorld
     sendMulti toLogic $ msgHeader :| [msgBody]
  where
    connections :: IO (Socket Pull, Socket Push)
    connections = do
        context'  <- context
        fromWorld <- createAndBind context' Pull anyHost portFromWorld
        toLogic   <- createAndBind context' Push anyHost portToLogic
        return (fromWorld, toLogic)

runSchedulerOut :: SchedulerConfig -> IO ()
runSchedulerOut SchedulerConfig{..} = do
  (fromLogic, toWorld) <- connections
  forever $ do
     [msgHeader, msgBody] <- receiveMulti fromLogic
     sendMulti toWorld $ msgHeader :| [msgBody]
  where
    connections :: IO (Socket Pull, Socket Pub)
    connections = do
        context'  <- context
        fromLogic <- createAndBind context' Pull anyHost portFromLogic
        toWorld   <- createAndBind context' Pub  anyHost portToWorld
        return (fromLogic, toWorld)

runSchedulerLogic :: SchedulerConfig -> IO ()
runSchedulerLogic SchedulerConfig{..} = do
  (toLogic, fromLogic) <- connections
  forever $ do
     [msgHeader, msgBody] <- receiveMulti toLogic
     sendMulti fromLogic $ msgHeader :| [msgBody]
  where
    connections :: IO (Socket Pull, Socket Push)
    connections = do
        context'  <- context
        toLogic <- createAndConnect context' Pull hostScheduler portToLogic
        fromLogic   <- createAndConnect context' Push hostScheduler portFromLogic
        return (toLogic, fromLogic)

