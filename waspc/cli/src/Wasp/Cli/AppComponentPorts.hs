module Wasp.Cli.AppComponentPorts
  ( defaultDevClientPort,
    defaultDevServerPort,
    findAppComponentPorts,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust)
import Network.Socket (PortNumber)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Port (checkIfLocalPortIsTaken, findFirstFreeLocalPort, maxNumOfPortsToCheck)
import Wasp.Util (whenM)

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
      assertPort
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
      assertPort
      requestedServerPort

  return (resolvedClientPort, resolvedServerPort)
  where
    assertPort port = do
      whenM (liftIO $ checkIfLocalPortIsTaken port) $ do
        throwResolvingError $ "Port " ++ show port ++ " is already in use."
      return port

    findPort startPort portsToSkip = do
      let candidatePorts = take maxNumOfPortsToCheck [startPort ..] \\ portsToSkip
      availablePort <- liftIO (findFirstFreeLocalPort candidatePorts)
      case availablePort of
        Nothing -> throwResolvingError $ noFreePortError candidatePorts
        Just port -> return port

    noFreePortError candidatePorts =
      "Wasp could not find a free port"
        ++ maybe "" showPortRange (NE.nonEmpty candidatePorts)
        ++ ". Free up some ports, or choose them yourself with --client-port and --server-port."
      where
        showPortRange ports = " in range " ++ show (NE.head ports) ++ "-" ++ show (NE.last ports)

    throwResolvingError = throwError . CommandError "Failed to find ports"
