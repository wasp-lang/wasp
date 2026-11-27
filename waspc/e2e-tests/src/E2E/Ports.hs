-- | Ports of the tests that run Wasp apps and other servers.
module E2E.Ports
  ( AppPorts (..),
    allocateAppPorts,
    appPortList,
    allocatePort,
  )
where

import Control.Concurrent.MVar (modifyMVar)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Set as Set
import E2E.Steps (TestEnv (..), TestM, logLine)
import E2E.Suite (TestSuiteEnv (..))
import qualified Network.Socket as Socket

data AppPorts = AppPorts
  { clientPort :: Int,
    serverPort :: Int
  }

-- | Ports that are free right now, so that tests can run in parallel with each
-- other, and with the tests of other checkouts on the same machine.
allocateAppPorts :: TestM AppPorts
allocateAppPorts = do
  ports <- AppPorts <$> allocateUnusedPort <*> allocateUnusedPort
  logLine $ "\n==> Allocated ports: client " ++ show ports.clientPort ++ ", server " ++ show ports.serverPort
  return ports

-- | A port that nothing listens on, and that no other test of this run got
-- yet (see 'E2E.Suite.allocatedPorts').
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
allocateUnusedPort :: TestM Int
allocateUnusedPort = do
  allocatedPorts <- asks (.testSuiteEnv.allocatedPorts)
  port <- liftIO findFreePort
  isNew <- liftIO . modifyMVar allocatedPorts $ \ports ->
    return (Set.insert port ports, port `Set.notMember` ports)
  if isNew then return port else allocateUnusedPort

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

-- | Like 'allocateAppPorts', for a command that takes a single port, e.g.
-- `wasp studio --port <port>`.
allocatePort :: TestM Int
allocatePort = do
  port <- allocateUnusedPort
  logLine $ "\n==> Allocated port " ++ show port
  return port
