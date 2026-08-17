module Wasp.Cli.Port where

import Network.Socket (PortNumber)
import Wasp.Util (ifM)
import qualified Wasp.Util.Network.Socket as S

-- | General setting for all of our logic that checks for free ports. This is
-- the maximum number of ports we should check for availability before giving up
-- and throwing an error.
maxNumOfPortsToCheck :: Int
maxNumOfPortsToCheck = 50

-- | Given a list of candidate ports, returns the first one that is available
-- for use.
firstAvailableLocalPort :: [PortNumber] -> IO (Maybe PortNumber)
firstAvailableLocalPort [] = return Nothing
firstAvailableLocalPort (x : xs) =
  ifM
    (isLocalPortTaken x)
    (firstAvailableLocalPort xs)
    (return $ Just x)

isLocalPortTaken :: PortNumber -> IO Bool
isLocalPortTaken port =
  -- We check both conditions because of Docker having a virtual network on Mac
  -- which always gives precedence to native ports, so checking only if we can
  -- open the port is not enough: we can open it even if a Docker container is
  -- already bound to it.
  S.checkIfPortIsInUse socketAddress >>= \case
    False -> S.checkIfPortIsAcceptingConnections socketAddress
    True -> return True
  where
    socketAddress = S.makeLocalHostSocketAddress port
