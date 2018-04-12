{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module System.MQ.Monad
  (
    MQMonad
  , MQError (..)
  , runMQMonad
  , errorHandler
  ) where

import           Control.Monad.Except (ExceptT, MonadError, MonadIO, liftIO,
                                       runExceptT)
import           System.Log.Logger    (errorM)
import           Text.Printf          (printf)

-- | 'MQMonad' is the base monad for the Monique System.
--
newtype MQMonad a = MQMonad { unMQMonad :: ExceptT MQError IO a }
  deriving ( Monad
           , Functor
           , Applicative
           , MonadIO
           , MonadError MQError
           )

-- | Turns 'MQMonad' into 'IO' monad.
-- If exception happens error will be thrown.
--
runMQMonad :: MQMonad a -> IO a
runMQMonad m = either renderError pure =<< runExceptT (unMQMonad m)
  where
    renderError :: MQError -> IO a
    renderError ms = ms `seq` print ms >> error "Shit happens"

-- | `errorHandler` just log message with error @err@ for the component with name @name@
--
errorHandler :: (Show e) => String -> e -> MQMonad ()
errorHandler name err = do
  liftIO . errorM name . show $! err
  pure ()

-- | 'MQError' is class for Monique Errors.
--
data MQError
  = MQProtocolError  { msg :: String }
  | MQTransportError { msg :: String }
  deriving (Eq, Ord)

instance Show MQError where
  show MQProtocolError{..}  = printf "Monique protocol error: %s" msg
  show MQTransportError{..} = printf "Monique transport error: %s" msg
