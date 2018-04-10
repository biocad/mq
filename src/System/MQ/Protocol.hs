module System.MQ.Protocol
  ( Timestamp
  , Hash
  , MessageTag
  , Message (..)
  , messageTag
  ) where

import           System.MQ.Protocol.Internal.Tag   (messageTag)
import           System.MQ.Protocol.Internal.Types (Hash, Message (..),
                                                    MessageTag, Timestamp)
