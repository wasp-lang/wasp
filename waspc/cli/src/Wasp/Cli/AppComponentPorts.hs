module Wasp.Cli.AppComponentPorts
  ( defaultDevClientPort,
    defaultDevServerPort,
    findAppComponentPorts,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes, isJust)
import Network.Socket (PortNumber)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Port (assertLocalPortIsFree, findFirstFreeLocalPortInRange)

defaultDevClientPort :: PortNumber
defaultDevClientPort = 3000

defaultDevServerPort :: PortNumber
defaultDevServerPort = 3001

findAppComponentPorts :: (Maybe PortNumber, Maybe PortNumber) -> Command (PortNumber, PortNumber)
findAppComponentPorts (requestedClientPort, requestedServerPort) = do
  let portsAreTheSame = isJust requestedClientPort && (requestedClientPort == requestedServerPort)
  when portsAreTheSame $ throwResolvingError "The client and the server can't both run on the same port."

  resolvedClientPort <-
    maybe
      (findPort defaultDevClientPort (catMaybes [requestedServerPort]))
      assertLocalPortIsFree
      requestedClientPort

  resolvedServerPort <-
    maybe
      ( findPort
          -- We already know all ports lower than the client port are taken, so
          -- we can start looking for a free port from the next one. This also
          -- has the nice effect of keeping the server port close to the client
          -- port.
          (resolvedClientPort + 1)
          []
      )
      assertLocalPortIsFree
      requestedServerPort

  return (resolvedClientPort, resolvedServerPort)
  where
    findPort startPort portsToSkip =
      liftIO
        ( findFirstFreeLocalPortInRange
            startPort
            portsToSkip
            "Free up some ports, or choose them yourself with --client-port and --server-port."
        )
        >>= either throwNoFreePortError return

    throwNoFreePortError :: String -> Command a
    throwNoFreePortError = throwError . CommandError "No free port"

    throwResolvingError = throwError . CommandError "Failed to find ports"
