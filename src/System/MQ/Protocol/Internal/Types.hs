{-# LANGUAGE DeriveGeneric #-}

module System.MQ.Protocol.Internal.Types
  (
    Timestamp
  , Hash
  , MessageTag
  , Message (..)
  ) where

import           Data.ByteString.Lazy          (ByteString)
import           Data.MessagePack              ()
import           Data.MessagePack.Types.Class  (MessagePack (..))
import           Data.MessagePack.Types.Object ()
import           GHC.Generics                  (Generic (..))

-- | Represents Unix epoch time in milliseconds.
--
type Timestamp  = Int

-- | Represents SHA-1 hash sum.
--
type Hash       = String

-- | Message tag is a bytestring with five separated with ':' fields: message type; spec; id; pid; creator.
-- See doc/PROTOCOL.md for more details.
--
type MessageTag = String

-- | Message is the main entity in MQ: various components, controllers and the Scheduler communicate with each other using Messages.
--
data Message
  = ConfigMessage { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: String
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: String
                  , msgConfig    :: ByteString
                  }
  | ResultMessage { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: String
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: String
                  , msgResult    :: ByteString
                  }
  | ErrorMessage  { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: String
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: String
                  , msgError     :: ByteString
                  }
  | DataMessage   { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: String
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: String
                  , msgData      :: ByteString
                  }
  deriving (Eq, Show, Read, Generic)

instance MessagePack Message

