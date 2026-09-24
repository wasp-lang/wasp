module Wasp.Cli.AppComponentPorts
  ( defaultDevClientPort,
    defaultDevServerPort,
    findAppComponentPorts,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Maybe (catMaybes, isJust)
import Network.Socket (PortNumber)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Port (resolvePort, useDifferentPortRemediation)

defaultDevClientPort :: PortNumber
defaultDevClientPort = 3000

defaultDevServerPort :: PortNumber
defaultDevServerPort = 3001

findAppComponentPorts :: (Maybe PortNumber, Maybe PortNumber) -> Command (PortNumber, PortNumber)
findAppComponentPorts (requestedClientPort, requestedServerPort) = do
  let portsAreTheSame = isJust requestedClientPort && (requestedClientPort == requestedServerPort)
  when portsAreTheSame $ throwResolvingError "The client and the server can't both run on the same port."

  resolvedClientPort <-
    resolvePort
      requestedClientPort
      defaultDevClientPort
      (catMaybes [requestedServerPort])
      (useDifferentPortRemediation "--client-port")
      noFreePortRemediation

  resolvedServerPort <-
    resolvePort
      requestedServerPort
      (resolvedClientPort + 1)
      []
      (useDifferentPortRemediation "--server-port")
      noFreePortRemediation

  return (resolvedClientPort, resolvedServerPort)
  where
    noFreePortRemediation = "Free up some ports, or choose them yourself with --client-port and --server-port."

    throwResolvingError = throwError . CommandError "Failed to find ports"
