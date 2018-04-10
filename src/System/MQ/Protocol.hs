module System.MQ.Protocol
  ( Timestamp
  , Hash
  , MessageTag
  , Message (..)
  , messageTag
  , module System.MQ.Protocol.Internal.Condition
  ) where

import           System.MQ.Protocol.Internal.Condition
import           System.MQ.Protocol.Internal.Tag       (messageTag)
import           System.MQ.Protocol.Internal.Types     (Hash, Message (..),
                                                        MessageTag, Timestamp)
