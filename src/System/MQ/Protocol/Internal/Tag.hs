module System.MQ.Protocol.Internal.Tag
  (
    messageTag
  ) where

import           Data.List                         (intercalate)
import           System.MQ.Protocol.Internal.Types (Message (..), MessageTag)

-- | Build a 'MessageTag' for the given message.
-- It is consists of five fields – message_type, spec, id, pid, creator – separated by ":".
-- See doc/PROTOCOL.md#Заголовок-сообщения for more information.
messageTag :: Message -> MessageTag
messageTag = intercalate ":" . ([messageType, msgSpec, msgId, msgPid, msgCreator] <*>) . pure

-- | Helper function which returns message type.
messageType :: Message -> String
messageType ConfigMessage {} = "config"
messageType ResultMessage {} = "result"
messageType ErrorMessage {}  = "error"
messageType DataMessage {}   = "data"
