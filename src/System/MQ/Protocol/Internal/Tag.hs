{-# LANGUAGE OverloadedStrings #-}

module System.MQ.Protocol.Internal.Tag
  (
    messageTag
  , messageType
  , messageSpec
  , messageId
  , messagePid
  , messageCreator
  , delimiter
  ) where

import           Data.ByteString                   (ByteString, intercalate,
                                                    split)
import           Data.Char                         (ord)
import           Data.String                       (IsString (..))
import           Data.Word                         (Word8)
import           System.MQ.Protocol.Internal.Types (Hash, Message (..),
                                                    MessageTag)

-- | Build a 'MessageTag' for the given message.
-- It is consists of five fields – message_type, spec, id, pid, creator – separated by ":".
-- See doc/PROTOCOL.md#Заголовок-сообщения for more information.
--
messageTag :: Message -> MessageTag
messageTag = intercalate ":" . ([msgType, fromString . msgSpec, msgId, msgPid, fromString . msgCreator] <*>) . pure

-- | Helper function which returns message type.
--
msgType :: Message -> ByteString
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

messageType :: MessageTag -> ByteString
messageType = head . split delimiter

messageSpec :: MessageTag -> ByteString
messageSpec = (!! 1) . split delimiter

messageId :: MessageTag -> Hash
messageId = (!! 2) . split delimiter

messagePid :: MessageTag -> Hash
messagePid = (!! 3) . split delimiter

messageCreator :: MessageTag -> ByteString
messageCreator = (!! 4) . split delimiter

delimiter :: Word8
delimiter = fromIntegral . ord $ ':'
