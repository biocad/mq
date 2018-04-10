
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module System.MQ.Scheduler
 (
    SchedulerConfig (..)
  , getSchedulerConfig
  , runSchedulerIn
  , runSchedulerOut
  , runSchedulerLogic
  ) where

import           Control.Monad              (forever)
import           Data.List.NonEmpty         (NonEmpty (..))

import           System.Log.Logger          (errorM, infoM)
import           System.MQ.Scheduler.Config (SchedulerConfig (..),
                                             getSchedulerConfig)
import           System.MQ.Transport        (Host, Port, anyHost, createAndBind,
                                             createAndConnect)
import           System.ZMQ4                (Pub (..), Pull (..), Push (..),
                                             Socket, context, receiveMulti,
                                             sendMulti)
import           Text.Printf                (printf)


-- | This module contains Scheduler, that is central place in Monique.
-- Scheduler consists from 3 parts:
--   * SchedulerIn
--
--
--
-- >             World
-- >               |
-- >  < PULL | SchedulerIn | PUSH >
-- >               |
-- > < PULL | SchedulerLogic | PUSH > x N
-- >               |
-- >  < PULL | SchedulerOut | PUB >
-- >               |
-- >             World
--

-- class Scheduler a where
--   type Connections a

--   connections :: a -> IO (Connections a)

--   rt :: a

  -- run :: Connections a -> IO ()


runSchedulerIn :: SchedulerConfig -> IO ()
runSchedulerIn SchedulerConfig{..} = do
  (fromWorld, toLogic) <- connections
  infoM "SchedulerIn" "Start working..."
  forever $ do
      msg <- receiveMulti fromWorld
      case msg of
          [msgHeader, msgBody] ->  sendMulti toLogic $ msgHeader :| [msgBody]
          _ -> errorM "SchedulerIn"$ printf "Expected message with [header, body]; received list with %d element(s)." (length msg)
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
  infoM "SchedulerOut" "Start working..."
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
  infoM "SchedulerLogic" "Start working..."
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

