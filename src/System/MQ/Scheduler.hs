{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module System.MQ.Scheduler
  ( Scheduler (run)
  , SchedulerIn (..)
  , SchedulerOut (..)
  , SchedulerLogic (..)
  , SchedulerConfig (..)
  , getSchedulerConfig
  ) where

import           Control.Monad              (forever)
import           Data.List.NonEmpty         (NonEmpty (..))
import           System.Log.Logger          (errorM, infoM)
import           System.MQ.Scheduler.Config (SchedulerConfig (..),
                                             getSchedulerConfig)
import           System.MQ.Transport        (anyHost, createAndBind,
                                             createAndConnect)
import           System.ZMQ4                (Pub (..), Pull (..), Push (..),
                                             Socket, context, receiveMulti,
                                             sendMulti)
import           Text.Printf                (printf)


-- | This module contains Scheduler, that is central place in Monique.
-- Scheduler consists from 3 parts:
--   * 'SchedulerIn'    - receives all messages from the world;
--   * 'SchedulerLogic' - process all messages with some logic;
--   * 'SchedulerOut'   - publishes all message to the world. 
--
-- Scheme:
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
-- As you can see, 'SchedulerLogic' can be represented with many copies, while 'SchedulerIn' and 'SchedulerOut' have only one.

class Scheduler a where

  -- | Inner type for connection sockets.
  --
  type Connections a

  -- | Function that initialize connection sockets.
  --
  connections :: a -> SchedulerConfig -> IO (Connections a)

  name :: a -> String

  -- | Running function for the component.
  --
  run :: a -> SchedulerConfig -> IO ()
  run a cfg = do
    connect <- connections a cfg
    infoM (name a) "Start working..."
    processing a connect

  -- | Function that defines component behavior
  --
  processing :: a -> Connections a -> IO ()


-- | 'SchedulerIn' receives all messages from the "world".
--
data SchedulerIn = SchedulerIn

instance Scheduler SchedulerIn where
  type Connections SchedulerIn = (Socket Pull, Socket Push)

  name _ = "SchedulerIn"

  connections _ SchedulerConfig{..} = do
      context'  <- context
      fromWorld <- createAndBind context' Pull anyHost portFromWorld
      toLogic   <- createAndBind context' Push anyHost portToLogic
      return (fromWorld, toLogic)

  processing _ (fromWorld, toLogic) = forever $ do
      msg <- receiveMulti fromWorld
      case msg of
          [msgHeader, msgBody] ->  sendMulti toLogic $ msgHeader :| [msgBody]
          _ -> errorM "SchedulerIn"$ printf "Expected message with [header, body]; received list with %d element(s)." (length msg)


-- | 'SchedulerOut' publishes message to the whole "world".
--
data SchedulerOut = SchedulerOut

instance Scheduler SchedulerOut where
  type Connections SchedulerOut = (Socket Pull, Socket Pub)

  name _ = "SchedulerOut"

  connections _ SchedulerConfig{..} = do
      context'  <- context
      fromLogic <- createAndBind context' Pull anyHost portFromLogic
      toWorld   <- createAndBind context' Pub  anyHost portToWorld
      return (fromLogic, toWorld)

  processing _ (fromLogic, toWorld) = forever $ do
     [msgHeader, msgBody] <- receiveMulti fromLogic
     sendMulti toWorld $ msgHeader :| [msgBody]


-- | 'SchedulerLogic' makes all the routine with messages.
--
data SchedulerLogic = SchedulerLogic

instance Scheduler SchedulerLogic where
  type Connections SchedulerLogic = (Socket Pull, Socket Push)

  name _ = "SchedulerLogic"

  connections _ SchedulerConfig{..} = do
      context'  <- context
      toLogic <- createAndConnect context' Pull hostScheduler portToLogic
      fromLogic   <- createAndConnect context' Push hostScheduler portFromLogic
      return (toLogic, fromLogic)

  processing _ (toLogic, fromLogic) = forever $ do
     [msgHeader, msgBody] <- receiveMulti toLogic
     sendMulti fromLogic $ msgHeader :| [msgBody]
