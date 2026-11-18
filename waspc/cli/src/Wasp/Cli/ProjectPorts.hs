module Wasp.Cli.ProjectPorts
  ( findProjectPorts,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (catMaybes, isJust)
import Network.Socket (PortNumber)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Port (resolvePort)
import Wasp.Cli.ProjectRunConfig (defaultDevClientPort)

findProjectPorts :: (Maybe PortNumber, Maybe PortNumber) -> Command (PortNumber, PortNumber)
findProjectPorts (requestedClientPort, requestedServerPort) = do
  let portsAreTheSame = isJust requestedClientPort && (requestedClientPort == requestedServerPort)
  when portsAreTheSame $ throwResolvingError "The client and the server can't both run on the same port."

  resolvedClientPort <-
    resolvePort
      requestedClientPort
      defaultDevClientPort
      (catMaybes [requestedServerPort])
      "Choose a different port with --client-port, or free up this one."
      noFreePortRemediation

  resolvedServerPort <-
    resolvePort
      requestedServerPort
      -- We already know all ports lower than the client port are taken, so
      -- we can start looking for a free port from the next one. This also
      -- has the nice effect of keeping the server port close to the client
      -- port.
      (resolvedClientPort + 1)
      []
      "Choose a different port with --server-port, or free up this one."
      noFreePortRemediation

  return (resolvedClientPort, resolvedServerPort)
  where
    noFreePortRemediation = "Free up some ports, or choose them yourself with --client-port and --server-port."

    throwResolvingError = throwError . CommandError "Failed to find ports"
