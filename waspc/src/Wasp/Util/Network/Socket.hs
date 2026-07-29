module Wasp.Util.Network.Socket
  ( checkIfLocalPortIsTaken,
    findFirstFreeLocalPort,
    checkIfPortIsAcceptingConnections,
    checkIfPortIsInUse,
    checkIfPortCanBeOpened,
    makeSocketAddress,
    makeLocalHostSocketAddress,
    IPv4,
  )
where

import Data.Word (Word8)
import Foreign.C.Error (Errno (..), eADDRINUSE, eCONNREFUSED)
import GHC.IO.Exception (IOException (..))
import qualified Network.Socket as S
import UnliftIO.Exception (bracket, throwIO, try)
import Wasp.Util (ifM)

-- | True if some process on this machine is already listening on the given port.
checkIfLocalPortIsTaken :: S.PortNumber -> IO Bool
checkIfLocalPortIsTaken port =
  -- We check both conditions because of Docker having a virtual network on Mac,
  -- which always gives precedence to native ports: checking only if we can open
  -- the port is not enough because we can open it even if a Docker container is
  -- already bound to it.
  ifM
    (checkIfPortIsInUse socketAddress)
    (return True)
    (checkIfPortIsAcceptingConnections socketAddress)
  where
    socketAddress = makeLocalHostSocketAddress port

-- | Returns the first port from the given list that no process on this machine
-- is listening on, or 'Nothing' if they are all taken.
findFirstFreeLocalPort :: [S.PortNumber] -> IO (Maybe S.PortNumber)
findFirstFreeLocalPort [] = return Nothing
findFirstFreeLocalPort (port : remainingPorts) =
  ifM
    (checkIfLocalPortIsTaken port)
    (findFirstFreeLocalPort remainingPorts)
    (return $ Just port)

-- | Tests if port is accepting connections.
-- Does so by trying to connect via socket to it (connection is closed immediately).
-- It returns True if connection succeeds, or False if connection is refused
-- (because port is not opened, nobody is listening on it).
-- Rethrows connection exceptions in all other cases (e.g. when the host
-- is unroutable).
checkIfPortIsAcceptingConnections :: S.SockAddr -> IO Bool
checkIfPortIsAcceptingConnections sockAddr = do
  bracket createSocket S.close' $ \sock ->
    try
      ( do
          S.connect sock sockAddr
      )
      >>= \case
        Right () -> return True
        Left e ->
          if isConnRefusedException e
            then return False
            else throwIO e
  where
    createSocket = createIPv4TCPSocket
    isConnRefusedException e = (Errno <$> ioe_errno e) == Just eCONNREFUSED

-- | True if port is in use, False if it is free, exception in all other cases.
checkIfPortIsInUse :: S.SockAddr -> IO Bool
checkIfPortIsInUse = (not <$>) . checkIfPortCanBeOpened

-- | Tests if port can be opened.
-- Does so by trying to bind a socket to it (and then closing it immediately).
-- Returns True if it can be opened, False if it is already in use, and throws
-- an exception in all other cases (e.g. when the host is unroutable).
checkIfPortCanBeOpened :: S.SockAddr -> IO Bool
checkIfPortCanBeOpened sockAddr = do
  bracket createSocket S.close' $ \sock ->
    try
      ( do
          S.bind sock sockAddr
          S.listen sock queueLength
      )
      >>= \case
        Right () -> return True
        Left e ->
          if isAddrInUseException e
            then return False
            else throwIO e
  where
    createSocket = do
      sock <- createIPv4TCPSocket
      S.setSocketOption sock S.ReuseAddr 1 -- Connect even if port is in TIME_WAIT state.
      return sock
    queueLength = 1
    isAddrInUseException e = (Errno <$> ioe_errno e) == Just eADDRINUSE

createIPv4TCPSocket :: IO S.Socket
createIPv4TCPSocket = S.socket S.AF_INET S.Stream S.defaultProtocol

-- | Creates a socket address from host IP and port number.
-- > makeSocketAddress (127,0,0,1) 8000
makeSocketAddress :: IPv4 -> S.PortNumber -> S.SockAddr
makeSocketAddress hostIp port = S.SockAddrInet port $ S.tupleToHostAddress hostIp

makeLocalHostSocketAddress :: S.PortNumber -> S.SockAddr
makeLocalHostSocketAddress = makeSocketAddress (127, 0, 0, 1)

type IPv4 = (Word8, Word8, Word8, Word8)
