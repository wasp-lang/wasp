module Wasp.Cli.Util.Port
  ( findFirstFreeLocalPort,
    defaultNumberOfPortsToScan,
  )
where

import Network.Socket (PortNumber)
import Wasp.Util (ifM)
import qualified Wasp.Util.Network.Socket as Socket

defaultNumberOfPortsToScan :: Int
defaultNumberOfPortsToScan = 20

findFirstFreeLocalPort :: [PortNumber] -> IO (Maybe PortNumber)
findFirstFreeLocalPort [] = return Nothing
findFirstFreeLocalPort (port : remainingPorts) =
  ifM
    (checkIfLocalPortIsTaken port)
    (findFirstFreeLocalPort remainingPorts)
    (return $ Just port)

checkIfLocalPortIsTaken :: PortNumber -> IO Bool
checkIfLocalPortIsTaken port =
  -- We check both conditions because of Docker having a virtual network on Mac
  -- which always gives precedence to native ports, so checking only if we can
  -- open the port is not enough: we can open it even if a Docker container is
  -- already bound to it.
  ifM
    (Socket.checkIfPortIsInUse socketAddress)
    (return True)
    (Socket.checkIfPortIsAcceptingConnections socketAddress)
  where
    socketAddress = Socket.makeLocalHostSocketAddress port
