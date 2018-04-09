module System.MQ.Transport
  (
    Host
  , Port
  , anyHost
  , localHost
  , showTCP
  , twinPort
  , createAndBind
  , createAndConnect
  ) where

import           System.ZMQ4 (Context, Socket, SocketType, bind, connect,
                              socket)
import           Text.Printf (printf)

-- | Creates 'Socket' and for given 'Context', 'SocketType', 'Host' and 'Port'.
-- This type of sockets is used when destination address is unknown.
--
createAndBind :: SocketType a => Context -> a -> Host -> Port -> IO (Socket a)
createAndBind = createAndAction bind

-- | Creates 'Socket' and for given 'Context', 'SocketType', 'Host' and 'Port'.
-- This type of sockets is used when destination address is known.
--
createAndConnect :: SocketType a => Context -> a -> Host -> Port -> IO (Socket a)
createAndConnect = createAndAction connect

-- | Inner function which 'connect's or 'bind's.
--
createAndAction :: SocketType a => (Socket a -> String -> IO ()) -> Context -> a -> Host -> Port -> IO (Socket a)
createAndAction action context socketType host port  = do
    socket' <- socket context socketType
    action socket' (showTCP host port)
    pure socket'

-- | Alias for host
--
type Host = String

-- | Alias for port
--
type Port = Int

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

-- | For given 'Port' returns ('Port', 'Port' + 1).
-- This is useful when "twin" ports are used for duplex connection.
--
twinPort :: Port -> (Port, Port)
twinPort port = (port, port + 1)
