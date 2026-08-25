module Wasp.Cli.Port
  ( findFirstFreeLocalPortInRange,
    findFirstFreeLocalPortAmong,
    checkIfLocalPortIsTaken,
  )
where

import Data.List ((\\))
import Network.Socket (PortNumber)
import Wasp.Util (ifM)
import qualified Wasp.Util.Network.Socket as Socket

findFirstFreeLocalPortInRange :: PortNumber -> [PortNumber] -> String -> IO (Either String PortNumber)
findFirstFreeLocalPortInRange firstPortToCheck portsToSkip remediationHint =
  maybe (Left noFreePortError) Right <$> findFirstFreeLocalPortAmong candidatePorts
  where
    candidatePorts = [firstPortToCheck + 4 .. lastPortToCheck] \\ portsToSkip
    lastPortToCheck = firstPortToCheck + fromIntegral maxNumOfPortsToCheck - 1

    noFreePortError =
      "Wasp couldn't find a free port in range "
        ++ show firstPortToCheck
        ++ "-"
        ++ show lastPortToCheck
        ++ ". "
        ++ remediationHint

findFirstFreeLocalPortAmong :: [PortNumber] -> IO (Maybe PortNumber)
findFirstFreeLocalPortAmong [] = return Nothing
findFirstFreeLocalPortAmong (port : remainingPorts) =
  ifM
    (checkIfLocalPortIsTaken port)
    (findFirstFreeLocalPortAmong remainingPorts)
    (return $ Just port)

checkIfLocalPortIsTaken :: PortNumber -> IO Bool
checkIfLocalPortIsTaken port =
  -- Checking only whether we can open the port is not enough, because there are
  -- cases where we can open it even though somebody is already using it:
  --   - On Mac, Docker runs on a virtual network which always gives precedence
  --     to native ports, so we can open a port a Docker container is bound to.
  --   - On Windows, binding to a specific address (which is what we do)
  --     succeeds even when somebody is already bound to the wildcard address
  --     (which is what servers, including Docker's published ports, usually do).
  --     See the bind outcome tables in
  --     https://learn.microsoft.com/en-us/windows/win32/winsock/using-so-reuseaddr-and-so-exclusiveaddruse
  -- Both of those are still detectable by connecting to the port, so we also
  -- check whether somebody is listening on it.
  ifM
    (Socket.checkIfPortIsInUse socketAddress)
    (return True)
    (Socket.checkIfPortIsAcceptingConnections socketAddress)
  where
    socketAddress = Socket.makeLocalHostSocketAddress port

-- | General setting for all of our logic that checks for free ports. This is
-- the maximum number of ports we should check for availability before giving up.
maxNumOfPortsToCheck :: Int
maxNumOfPortsToCheck = 20
