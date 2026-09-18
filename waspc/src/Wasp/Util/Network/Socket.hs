module Wasp.Util.Network.Socket
  ( ConnectProbeResult (..),
    classifyConnectException,
    probeConnectResult,
    checkIfPortIsAcceptingConnections,
    checkIfPortIsInUse,
    checkIfPortCanBeOpened,
    makeSocketAddress,
    makeLocalHostSocketAddress,
    IPv4,
  )
where

import Data.List (isInfixOf)
import Data.Word (Word8)
import Foreign.C.Error (Errno (..), eADDRINUSE, eCONNREFUSED, eTIMEDOUT)
import GHC.IO.Exception (IOException (..))
import qualified Network.Socket as S
import UnliftIO.Exception (bracket, throwIO, try)

-- | The definitive outcome of probing a port with a TCP connect.
data ConnectProbeResult
  = -- | The connection was accepted: somebody is listening on the port.
    ConnectAccepted
  | -- | The connection was refused: nobody is listening on the port.
    ConnectRefused
  | -- | The connection attempt timed out: something is holding the port
    -- without answering (e.g. `nc -l` on macOS, or a firewall silently
    -- dropping packets), so the port is not usable even though nobody
    -- properly listens on it.
    ConnectTimedOut
  deriving (Eq, Show)

-- | Tests what state a port is in by trying to connect to it (the connection
-- is closed immediately).
-- Rethrows connection exceptions whose outcome is indeterminate (e.g.
-- insufficient permissions, unroutable host, resource exhaustion), since
-- those say nothing about whether somebody is listening on the port.
probeConnectResult :: S.SockAddr -> IO ConnectProbeResult
probeConnectResult sockAddr = do
  bracket createIPv4TCPSocket S.close' $ \sock ->
    try (S.connect sock sockAddr)
      >>= \case
        Right () -> return ConnectAccepted
        Left e -> either throwIO return $ classifyConnectException e

-- | Classifies a connect() exception into a definitive probe outcome when
-- possible, so the caller can decide what it means for the port's state.
-- Kept pure (and exported) so the classification rules can be tested directly.
--
-- On Windows, the exception doesn't have the errno field set at all: the
-- network package puts a WinSock error name in the exception's description
-- instead, so we have to check for both.
classifyConnectException :: IOException -> Either IOException ConnectProbeResult
classifyConnectException e
  | isConnRefusedException e = Right ConnectRefused
  | isTimeoutException e = Right ConnectTimedOut
  | otherwise = Left e

isConnRefusedException :: IOException -> Bool
isConnRefusedException e =
  (Errno <$> ioe_errno e) == Just eCONNREFUSED
    || "WSAECONNREFUSED" `isInfixOf` ioe_description e

isTimeoutException :: IOException -> Bool
isTimeoutException e =
  (Errno <$> ioe_errno e) == Just eTIMEDOUT
    || "WSAETIMEDOUT" `isInfixOf` ioe_description e

-- | Tests if port is accepting connections.
-- Does so by trying to connect via socket to it (connection is closed immediately).
-- It returns True if connection succeeds, or False if connection is refused
-- (because port is not opened, nobody is listening on it).
-- Rethrows connection exceptions in all other cases (e.g. when the host
-- is unroutable or the connection times out). Callers that want to treat a
-- timed out connection as "taken" should use 'probeConnectResult' instead.
checkIfPortIsAcceptingConnections :: S.SockAddr -> IO Bool
checkIfPortIsAcceptingConnections sockAddr = do
  bracket createIPv4TCPSocket S.close' $ \sock ->
    try (S.connect sock sockAddr)
      >>= \case
        Right () -> return True
        Left e ->
          if isConnRefusedException e
            then return False
            else throwIO e

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
      -- Lets us bind even if the port is in TIME_WAIT state. On Windows, this
      -- is also what makes binding to a taken port report WSAEACCES instead of
      -- WSAEADDRINUSE (see 'isAddrInUseException' below).
      S.setSocketOption sock S.ReuseAddr 1
      return sock
    queueLength = 1
    -- On Windows, the exception doesn't have the errno field set at all: the
    -- network package puts a WinSock error name in the exception's description
    -- instead, so we have to check for both.
    --
    -- Windows reports our bind over a taken port as WSAEACCES rather than
    -- WSAEADDRINUSE, since we ask for address reuse while the socket holding
    -- the port didn't opt into it. See the bind outcome tables in
    -- https://learn.microsoft.com/en-us/windows/win32/winsock/using-so-reuseaddr-and-so-exclusiveaddruse
    isAddrInUseException e =
      (Errno <$> ioe_errno e) == Just eADDRINUSE
        || any (`isInfixOf` ioe_description e) ["WSAEADDRINUSE", "WSAEACCES"]

createIPv4TCPSocket :: IO S.Socket
createIPv4TCPSocket = S.socket S.AF_INET S.Stream S.defaultProtocol

-- | Creates a socket address from host IP and port number.
-- > makeSocketAddress (127,0,0,1) 8000
makeSocketAddress :: IPv4 -> S.PortNumber -> S.SockAddr
makeSocketAddress hostIp port = S.SockAddrInet port $ S.tupleToHostAddress hostIp

makeLocalHostSocketAddress :: S.PortNumber -> S.SockAddr
makeLocalHostSocketAddress = makeSocketAddress (127, 0, 0, 1)

type IPv4 = (Word8, Word8, Word8, Word8)
