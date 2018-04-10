module System.MQ.Protocol.Internal.Tag
  (
    messageTag
  , messageType
  , messageSpec
  , messageId
  , messagePid
  , messageCreator
  ) where

import           Data.List                         (intercalate)
import           Data.List.Split                   (splitOn)
import           System.MQ.Protocol.Internal.Types (Hash, Message (..),
                                                    MessageTag)

-- | Build a 'MessageTag' for the given message.
-- It is consists of five fields – message_type, spec, id, pid, creator – separated by ":".
-- See doc/PROTOCOL.md#Заголовок-сообщения for more information.
messageTag :: Message -> MessageTag
messageTag = intercalate ":" . ([msgType, msgSpec, msgId, msgPid, msgCreator] <*>) . pure

-- | Helper function which returns message type.
msgType :: Message -> String
msgType ConfigMessage {} = "config"
msgType ResultMessage {} = "result"
msgType ErrorMessage {}  = "error"
msgType DataMessage {}   = "data"

-- | Filtration:
-- Use System.MQ.Protocol.Internal.Condition
-- > "foo:bar:baz:ss:er" `matches` (messageType :== "foo" :&& messageId :== "baz")
-- > True
-- > "foo:bar:baz:ss:er" `matches` (messageType :== "foo" :&& messageId :== "ba")
-- > False

messageType :: MessageTag -> String
messageType = head . splitOn ":"

messageSpec :: MessageTag -> String
messageSpec = (!! 1) . splitOn ":"

messageId :: MessageTag -> Hash
messageId = (!! 2) . splitOn ":"

messagePid :: MessageTag -> Hash
messagePid = (!! 3) . splitOn ":"

messageCreator :: MessageTag -> String
messageCreator = (!! 4) . splitOn ":"

