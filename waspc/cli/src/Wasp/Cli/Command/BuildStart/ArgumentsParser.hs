module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.EnvVarArgument (envVarFileParser, envVarInlineParser)
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
    <$> environmentVariableParsersForComponent "client"
    <*> environmentVariableParsersForComponent "server"
    <*> portOption "client-port" "Port to run the client on"
    <*> portOption "server-port" "Port to run the server on"
  where
    environmentVariableParsersForComponent name =
      liftA2
        (,)
        (envVarInlinesParserForComponent name)
        (envVarFilesParserForComponent name)

    envVarInlinesParserForComponent name =
      Opt.many $
        envVarInlineParser
          (head name) -- e.g. "-c"
          (name ++ "-env") -- e.g. "--client-env"
          ("Set an environment variable for the " ++ name ++ " (can be used multiple times)")

    envVarFilesParserForComponent name =
      Opt.many $
        envVarFileParser
          (name ++ "-env-file") -- e.g. "--client-env-file"
          ("Load environment variables for the " ++ name ++ " from a file (can be used multiple times)")
