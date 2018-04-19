{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module System.MQ.Protocol.Internal.Types
  (
    Timestamp
  , Hash
  , Creator
  , Encoding
  , Spec
  , MessageTag
  , Message (..)
  , MessageType (..)
  , Dictionary (..)
  ) where

import           Data.ByteString               as BS (ByteString)
import           Data.Map.Strict               (Map)
import           Data.MessagePack              ()
import           Data.MessagePack.Types.Object (Object)
import           GHC.Generics                  (Generic (..))

-- | Dictionary class describes objects that can be turned into an association list (key := value) Object
--
class Dictionary a where
  toDictionary :: a -> Map ByteString Object
  fromDictionary :: Monad m => Map ByteString Object -> m a

-- | Represents Unix epoch time in milliseconds.
--
type Timestamp  = Int

-- | Represents SHA-1 hash sum.
--
type Hash       = ByteString

-- | Message tag is a 'ByteString' with five separated with ':' fields: message type; spec; id; pid; creator.
-- It can be created from 'Message' automatically.
-- See doc/PROTOCOL.md for more details.
--
type MessageTag = ByteString

-- | Creator is identifier for user or system id that creates message.
--
type Creator    = String

-- | Encoding is message encoding type. For this moment it can be "JSON" or "MessagePack".
--
type Encoding   = String

-- | Spec is message specification.
--
type Spec       = String

-- | 'Message' is the main entity in MQ: various components, controllers and the Scheduler communicate with each other using 'Message's.
--
data Message
  = ConfigMessage { msgId        :: Hash        -- ^ bin format family
                  , msgPid       :: Hash        -- ^ bin format family
                  , msgCreator   :: Creator     -- ^ str format family
                  , msgCreatedAt :: Timestamp   -- ^ int format family
                  , msgExpiresAt :: Timestamp   -- ^ int format family
                  , msgSpec      :: Spec        -- ^ str format family
                  , msgEncoding  :: Encoding    -- ^ str format family
                  , msgConfig    :: ByteString  -- ^ bin format family
                  }
  | ResultMessage { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: Creator
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: Spec
                  , msgEncoding  :: Encoding
                  , msgResult    :: ByteString
                  }
  | ErrorMessage  { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: Creator
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: Spec
                  , msgEncoding  :: Encoding
                  , msgError     :: ByteString
                  }
  | DataMessage   { msgId        :: Hash
                  , msgPid       :: Hash
                  , msgCreator   :: Creator
                  , msgCreatedAt :: Timestamp
                  , msgExpiresAt :: Timestamp
                  , msgSpec      :: Spec
                  , msgEncoding  :: Encoding
                  , msgData      :: ByteString
                  }

  deriving (Eq, Show, Read, Generic)

-- | 'MessageType' describes valid message types in Monique.
--
data MessageType = Config | Result | Error | Data
