module Wasp.Cli.Util.PortArgument
  ( resolveAppPorts,
    defaultAppPorts,
    appPortsParser,
    portOption,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List ((\\))
import Data.Maybe (catMaybes, isJust)
import Data.Traversable (for)
import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Project.PerService (PerService (..))
import qualified Wasp.Project.PerService as PerService
import Wasp.Util (ifM, whenM)
import qualified Wasp.Util.Network.Socket as S

appPortsParser :: Opt.Parser (PerService (Maybe PortNumber))
appPortsParser =
  for PerService.names $ \name ->
    portOption (name ++ "-port") ("Port to run the " ++ name ++ " on")

portOption :: String -> String -> Opt.Parser (Maybe PortNumber)
portOption optionName helpText =
  Opt.optional $
    Opt.option
      (Opt.auto >>= rejectAnyPort)
      ( Opt.long optionName
          <> Opt.metavar "PORT"
          <> Opt.help helpText
      )
  where
    -- Reading into a 'PortNumber' already rejects anything outside 1-65535,
    -- except for 0, which means "let the OS pick a port". We can't work with
    -- that, since we have to tell the other side where this one is running.
    rejectAnyPort 0 = Opt.readerError "0 is not a valid port"
    rejectAnyPort port = return port

defaultMinPort :: PortNumber
defaultMinPort = 3000

defaultAppPorts :: PerService PortNumber
defaultAppPorts = PerService defaultMinPort (defaultMinPort + 1)

resolveAppPorts :: PerService (Maybe PortNumber) -> Command (PerService PortNumber)
resolveAppPorts requestedPorts = do
  let portsAreTheSame = isJust requestedPorts.client && (requestedPorts.client == requestedPorts.server)
  when portsAreTheSame $ throwResolvingError "The client and the server can't both run on the same port."

  resolvedClientPort <-
    resolvePort
      requestedPorts.client
      defaultMinPort
      (catMaybes [requestedPorts.server])

  resolvedServerPort <-
    resolvePort
      requestedPorts.server
      (resolvedClientPort + 1)
      (catMaybes [requestedPorts.client])

  return $ PerService resolvedClientPort resolvedServerPort
  where
    resolvePort (Just port) _ _ = do
      whenM (liftIO $ isLocalPortTaken port) $ do
        throwResolvingError $ "Port " ++ show port ++ " is already in use."
      return port
    resolvePort Nothing startPort removePorts = do
      let candidatePorts = take maxPortCheck [startPort ..] \\ removePorts
      availablePort <- liftIO (firstAvailableLocalPort candidatePorts)
      case availablePort of
        Nothing -> throwResolvingError $ noFreePortError candidatePorts
        Just port -> return port

    noFreePortError candidatePorts =
      "Wasp could not find a free port in range "
        ++ show (head candidatePorts)
        ++ "-"
        ++ show (last candidatePorts)
        ++ ". Free up some ports, or choose them yourself with --client-port and --server-port."

    throwResolvingError = throwError . CommandError "Failed to resolve app ports"

    maxPortCheck :: Int
    maxPortCheck = 50

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
