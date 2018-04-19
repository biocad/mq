module System.MQ.Transport
  (
    module System.MQ.Transport.Internal.Types
  , push
  , pull
  , pub
  , sub
  ) where

import           System.MQ.Encoding.MessagePack         (pack, unpackM)
import           System.MQ.Monad                        (MQMonad)
import           System.MQ.Protocol                     (Message, MessageTag)
import           System.MQ.Protocol.Internal.Instances  ()
import qualified System.MQ.Transport.ByteString         as TBS (pub, pull, push,
                                                                sub)
import           System.MQ.Transport.Internal.Instances ()
import           System.MQ.Transport.Internal.Types

-- | Pushes @(tag, content)@ to the 'PushChannel'.
--
push :: PushChannel -> (MessageTag, Message) -> MQMonad ()
push channel (tag, content) = TBS.push channel (pack tag, pack content)

-- | Pulls @(tag, content)@ from the 'PullChannel'.
--
pull :: PullChannel -> MQMonad (MessageTag, Message)
pull channel = do
  (tag, content) <- TBS.pull channel
  utag           <- unpackM tag
  ucontent       <- unpackM content
  pure (utag, ucontent)

-- | Publishes @(tag, content)@ to the 'PubChannel'.
--
pub :: PubChannel -> (MessageTag, Message) -> MQMonad ()
pub channel (tag, content) = TBS.pub channel (pack tag, pack content)

-- | Subscribes and gets @(tag, content)@ from the 'SubChannel'.
--
sub :: SubChannel -> MQMonad (MessageTag, Message)
sub channel = do
  (tag, content) <- TBS.sub channel
  utag           <- unpackM tag
  ucontent       <- unpackM content
  pure (utag, ucontent)


