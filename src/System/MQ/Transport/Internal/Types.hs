module System.MQ.Transport.Internal.Types
  (
    Host
  , Port
  , HostPort (..)
  , PushChannel
  , PullChannel
  , PubChannel
  , SubChannel
  , ConnectTo (..)
  , BindTo (..)
  , System.ZMQ4.Context
  , System.ZMQ4.context
  , anyHost
  , localHost
  , showTCP
  ) where

import           System.ZMQ4 (Context, Pub, Pull, Push, Socket, Sub, context)
import           Text.Printf (printf)

-- | Alias for host
--
type Host = String

-- | Alias for port
--
type Port = Int

-- | Container for 'Host' and 'Port'
--
data HostPort = HostPort { host :: Host
                         , port :: Port
                         }

-- | Channel to 'Push' messages
--
type PushChannel = Socket Push

-- | Channel to 'Pull' messages
--
type PullChannel = Socket Pull

-- | Channel to 'Pub' messages
--
type PubChannel  = Socket Pub

-- | Channel to 'Sub' messages
--
type SubChannel  = Socket Sub

-- | Class for connecting to the given 'HostPort'.
--
class ConnectTo a where
  connectTo :: HostPort -> Context -> IO a

-- | Class for bind to the given 'HostPort'.
--
class BindTo a where
  bindTo :: HostPort -> Context -> IO a

-- | Sometimes wildcald host is needed for connections
--
anyHost :: Host
anyHost = "*"

-- | Alias for localhost
--
localHost :: Host
localHost = "127.0.0.1"

-- | Converts 'Host' and 'Port' into tcp adress
--
showTCP :: Host -> Port -> String
showTCP = printf "tcp://%s:%d"
