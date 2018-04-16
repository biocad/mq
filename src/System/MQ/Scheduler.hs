module System.MQ.Scheduler
  ( module System.MQ.Scheduler.Internal.Config
  , module System.MQ.Scheduler.Internal.In
  , module System.MQ.Scheduler.Internal.Logic
  , module System.MQ.Scheduler.Internal.Out
  ) where

import System.MQ.Scheduler.Internal.Config
import System.MQ.Scheduler.Internal.In
import System.MQ.Scheduler.Internal.Logic
import System.MQ.Scheduler.Internal.Out 

--------------------------------------------------------------------------------
--  This module contains Scheduler, that is central place in Monique.
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
--------------------------------------------------------------------------------

{-





-- | General class for all type of 'Scheduler's.
--
class Scheduler a where

  -- | Inner type for connection sockets.
  type Connections a

  -- | Function that initialize connection sockets.
  connections :: a -> SchedulerConfig -> IO (Connections a)

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

--------------------------------------------------------------------------------
-- INSTANCES
--------------------------------------------------------------------------------

-- | 'SchedulerIn' receives all messages from the "world".
--
data SchedulerIn = SchedulerIn

instance Scheduler SchedulerIn where
  type Connections SchedulerIn = (PullChannel, PushChannel)

  name _ = "SchedulerIn"

  connections _ SchedulerConfig{..} = do
      context'  <- context
      fromWorld <- bindTo fromWorldHP context'
      toLogic   <- bindTo toLogicHP context'
      return (fromWorld, toLogic)
    where
      fromWorldHP = HostPort anyHost (port schedulerInOuter)
      toLogicHP   = HostPort anyHost (port schedulerInInner)

  processing _ (fromWorld, toLogic) = pull fromWorld >>= (`push` toLogic)

-- | 'SchedulerOut' publishes message to the whole "world".
--
data SchedulerOut = SchedulerOut

instance Scheduler SchedulerOut where
  type Connections SchedulerOut = (PullChannel, PubChannel)

  name _ = "SchedulerOut"

  connections _ SchedulerConfig{..} = do
      context'  <- context
      fromLogic <- bindTo fromLogicHP context'
      toWorld   <- bindTo toWorldHP context'
      return (fromLogic, toWorld)
    where
      fromLogicHP = HostPort anyHost (port schedulerOutInner)
      toWorldHP   = HostPort anyHost (port schedulerOutOuter)

  processing _ (fromLogic, toWorld) = pull fromLogic >>= (`pub` toWorld)

-- | 'SchedulerLogic' makes all the routine with messages.
--
data SchedulerLogic = SchedulerLogic

instance Scheduler SchedulerLogic where
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
