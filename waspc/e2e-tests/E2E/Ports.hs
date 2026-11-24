-- | Ports of the tests that run Wasp apps.
module E2E.Ports
  ( AppPorts,
    allocateAppPorts,
    appPortList,
    appPortArgs,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Set (Set)
import qualified Data.Set as Set
import E2E.RunConfig (RunConfig (..))
import E2E.TestM (TestEnv (..), TestM, logLine)
import qualified Network.Socket as Socket

data AppPorts = AppPorts
  { clientPort :: Int,
    serverPort :: Int
  }

-- | Ports that are free right now, so that tests can run in parallel with each
-- other, and with the tests of other checkouts on the same machine.
allocateAppPorts :: TestM AppPorts
allocateAppPorts = do
  allocatedPorts <- asks (.runConfig.allocatedPorts)
  ports <- liftIO $ AppPorts <$> allocatePort allocatedPorts <*> allocatePort allocatedPorts
  logLine $ "\n==> Allocated ports: client " ++ show ports.clientPort ++ ", server " ++ show ports.serverPort
  return ports

-- | A port that nothing listens on, and that no other test of this run got
-- yet (see 'allocatedPorts').
--
-- We close the socket before the test's app opens the port, and the OS can
-- hand a closed port out again, so we remember the ports we gave out to keep
-- two tests of this run from getting the same one.
--
-- Anything else on the machine can still take the port before the test's app
-- opens it. That window lasts until the app starts listening, which can be
-- minutes (e.g. after `npm install`), and the port is in the range the OS
-- picks outgoing connections' ports from. Then the app fails to start, with an
-- "address in use" error in the test's log.
allocatePort :: MVar (Set Int) -> IO Int
allocatePort allocatedPorts = do
  port <- findFreePort
  isNew <- modifyMVar allocatedPorts $ \ports ->
    return (Set.insert port ports, port `Set.notMember` ports)
  if isNew then return port else allocatePort allocatedPorts

-- | Lets the OS pick a free port. The socket is dual-stack, so the port is
-- free on both IPv4 and IPv6: Vite e.g. listens on only one of them.
findFreePort :: IO Int
findFreePort =
  bracket (Socket.socket Socket.AF_INET6 Socket.Stream Socket.defaultProtocol) Socket.close $ \socket -> do
    Socket.setSocketOption socket Socket.IPv6Only 0
    Socket.bind socket (Socket.SockAddrInet6 0 0 (0, 0, 0, 0) 0)
    fromIntegral <$> Socket.socketPort socket

appPortList :: AppPorts -> [Int]
appPortList ports = [ports.clientPort, ports.serverPort]

-- | For `wasp start` and `wasp build start`.
appPortArgs :: AppPorts -> [String]
appPortArgs ports = ["--client-port", show ports.clientPort, "--server-port", show ports.serverPort]
