module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import Data.Maybe (fromMaybe)
import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.AppComponentPorts (defaultDevClientPort, defaultDevServerPort)
import Wasp.Cli.Util.EnvVarArgument (EnvVarArgument, envVarArgumentFileParser, envVarArgumentLiteralParser)
import Wasp.Cli.Util.PortArgument (portOption)

data BuildStartArgs = BuildStartArgs
  { -- | Kept optional so we can tell the user it is ignored in single deployment mode.
    clientPort :: Maybe PortNumber,
    serverPort :: PortNumber,
    clientEnvVars :: [EnvVarArgument],
    serverEnvVars :: [EnvVarArgument]
  }

buildStartArgsParser :: Opt.Parser BuildStartArgs
buildStartArgsParser =
  BuildStartArgs
    <$> portParserForComponent "client" defaultDevClientPort
    <*> (fromMaybe defaultDevServerPort <$> portParserForComponent "server" defaultDevServerPort)
    <*> environmentVariableParsersForComponent 'c' "client"
    <*> environmentVariableParsersForComponent 's' "server"
  where
    portParserForComponent name defaultPort =
      portOption
        (name ++ "-port") -- e.g. "--client-port"
        ("Port to run the " ++ name ++ " on (default: " ++ show defaultPort ++ ")")

    environmentVariableParsersForComponent shortOptionName name =
      liftA2
        (<>)
        (envVarInlinesParserForComponent shortOptionName name)
        (envVarFilesParserForComponent name)

    envVarInlinesParserForComponent shortOptionName name =
      Opt.many $
        envVarArgumentLiteralParser
          shortOptionName -- e.g. 'c', for "-c"
          (name ++ "-env") -- e.g. "--client-env"
          ("Set an environment variable for the " ++ name ++ " (can be used multiple times)")

    envVarFilesParserForComponent name =
      Opt.many $
        envVarArgumentFileParser
          (name ++ "-env-file") -- e.g. "--client-env-file"
          ("Load environment variables for the " ++ name ++ " from a file (can be used multiple times)")
