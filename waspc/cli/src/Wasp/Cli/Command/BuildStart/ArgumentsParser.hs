module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.EnvVarArgument (envVarArgumentsParser, envVarFilesParser)
import Wasp.Cli.Util.PathArgument (FilePathArgument)
import Wasp.Cli.Util.PortArgument (portOption)
import Wasp.Env (EnvVar)

data BuildStartArgs = BuildStartArgs
  { clientEnvVarSources :: ([EnvVar], [FilePathArgument]),
    serverEnvVarSources :: ([EnvVar], [FilePathArgument]),
    clientPort :: Maybe PortNumber,
    serverPort :: Maybe PortNumber
  }

buildStartArgsParser :: Opt.Parser BuildStartArgs
buildStartArgsParser =
  BuildStartArgs
    <$> environmentVariableParser "client" 'c' "client-env" "client-env-file"
    <*> environmentVariableParser "server" 's' "server-env" "server-env-file"
    <*> portOption "client-port" "Port to run the client on"
    <*> portOption "server-port" "Port to run the server on"
  where
    environmentVariableParser targetName shortOptionName longOptionName fileOptionName =
      liftA2
        (,)
        (envVarArgumentsParser targetName shortOptionName longOptionName)
        (envVarFilesParser targetName fileOptionName)
