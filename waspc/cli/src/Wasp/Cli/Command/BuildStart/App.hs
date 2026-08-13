module Wasp.Cli.Command.BuildStart.App
  ( buildApp,
    startApp,
  )
where

import Data.Function ((&))
import qualified StrongPath as SP
import System.Process (proc)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import Wasp.Env (EnvVar)
import Wasp.Generator.DockerGenerator (clientEnvBuildArgName)
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runProcessAsJob)

buildApp :: BuildStartConfig -> ExceptJob
buildApp config =
  runProcessAsJob
    (proc "docker" (["build", "--tag", dockerImageName] <> clientEnvParams <> [dockerContextDir]))
    J.App
    & toExceptJob (("Building the app failed with exit code: " <>) . show)
  where
    dockerContextDir = SP.fromAbsDir $ Config.buildDir config
    dockerImageName = Config.dockerImageName config

    -- The client's environment variables are part of the app's pages and
    -- assets, which the image build is what produces.
    clientEnvParams = toBuildArgParams (Config.clientEnvVars config)

startApp :: BuildStartConfig -> ExceptJob
startApp config =
  runProcessAsJob
    ( proc
        "docker"
        ( ["run", "--name", dockerContainerName, "--rm", "--network", "host"]
            <> serverEnvParams
            <> [dockerImageName]
        )
    )
    J.App
    & toExceptJob (("Running the app failed with exit code: " <>) . show)
  where
    serverEnvParams = toEnvVarParams $ Config.serverEnvVars config
    dockerContainerName = Config.dockerContainerName config
    dockerImageName = Config.dockerImageName config

toEnvVarParams :: [EnvVar] -> [String]
toEnvVarParams envVars = envVars >>= \(name, value) -> ["--env", name <> "=" <> value]

-- | Docker only passes build arguments a Dockerfile declares, and the names of
-- the client's environment variables aren't known when we generate it. So they
-- all travel in one build argument, as shell assignments the Dockerfile
-- evaluates.
toBuildArgParams :: [EnvVar] -> [String]
toBuildArgParams [] = []
toBuildArgParams envVars =
  ["--build-arg", clientEnvBuildArgName <> "=" <> unlines (map toShellAssignment envVars)]
  where
    toShellAssignment (name, value) = name <> "=" <> singleQuote value

    -- A single-quoted shell word ends at the first quote, so a quote inside the
    -- value is written by closing the quotes, escaping it, and opening them
    -- again.
    singleQuote value = "'" <> concatMap escapeSingleQuote value <> "'"
    escapeSingleQuote '\'' = "'\\''"
    escapeSingleQuote character = [character]
