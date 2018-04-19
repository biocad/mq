module System.MQ.Protocol.Class
  (
    MessageLike (..)
  , Props (..)
  ) where

import           System.MQ.Protocol (Encoding, MessageType, Spec)

-- | Every message in Monique system has fixed properties, that can be describe with type 'Props'.
--
data Props a = Props { spec     :: Spec        -- ^ message spec
                     , mtype    :: MessageType -- ^ message type (Config, Result, Error or Data)
                     , encoding :: Encoding    -- ^ message encoding
                     }

-- | 'MessageLike' connects meaningful information about message with it's properties.
-- "meaningful information" is 'ByteString' that is stored in one of the following message fields: config, result, error or data.
-- But content of the 'ByteString' is not enough to describe specification, type and encoding of the message.
-- So every message in haskell language should be instance of 'MessageLike'.
--
class MessageLike a where
    props :: Props a


