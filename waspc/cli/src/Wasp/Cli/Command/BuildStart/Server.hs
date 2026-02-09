module Wasp.Cli.Command.BuildStart.Server
  ( buildServer,
    startServer,
  )
where

import Data.Function ((&))
import Data.List (isInfixOf)
import qualified StrongPath as SP
import System.Process (proc)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import Wasp.Generator.ServerGenerator.Common (defaultServerPort)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runProcessAsJob)

buildServer :: BuildStartConfig -> ExceptJob
buildServer config =
  runProcessAsJob
    (proc "docker" ["build", "--tag", dockerImageName, dockerContextDir])
    J.Server
    & toExceptJob (("Building the server failed with exit code: " <>) . show)
  where
    dockerContextDir = SP.fromAbsDir buildDir
    buildDir = Config.buildDir config
    dockerImageName = Config.dockerImageName config

-- | Start the server Docker container.
-- Uses `-p PORT:PORT` instead of `--network host` because the latter
-- does not expose ports on macOS Docker Desktop.
-- Env vars with `localhost` are rewritten to `host.docker.internal`
-- so the container can reach host services (DB, SMTP, etc.), except
-- for WASP_SERVER_URL and WASP_WEB_CLIENT_URL which are browser-facing.
startServer :: BuildStartConfig -> ExceptJob
startServer config =
  runProcessAsJob
    ( proc
        "docker"
        ( [ "run",
            "--name",
            dockerContainerName,
            "--rm",
            "-p",
            portMapping,
            "--add-host=host.docker.internal:host-gateway"
          ]
            <> envVarParams
            <> [dockerImageName]
        )
    )
    J.Server
    & toExceptJob (("Running the server failed with exit code: " <>) . show)
  where
    port = show defaultServerPort
    portMapping = port <> ":" <> port

    -- Env vars whose values should NOT have localhost rewritten.
    -- These are browser-facing URLs that must remain as localhost.
    browserFacingEnvVars =
      [ Server.serverUrlEnvVarName, -- WASP_SERVER_URL
        Server.clientUrlEnvVarName -- WASP_WEB_CLIENT_URL
      ]

    -- Rewrite localhost -> host.docker.internal for env vars that
    -- reference host services (DATABASE_URL, SMTP_HOST, etc.),
    -- but leave browser-facing URLs untouched.
    rewriteLocalhostForDocker (name, value)
      | name `elem` browserFacingEnvVars = (name, value)
      | "localhost" `isInfixOf` value = (name, replaceLocalhost value)
      | otherwise = (name, value)

    replaceLocalhost [] = []
    replaceLocalhost s
      | take 9 s == "localhost" =
          "host.docker.internal" <> replaceLocalhost (drop 9 s)
      | otherwise = head s : replaceLocalhost (tail s)

    envVarParams = toEnvVarParams $ map rewriteLocalhostForDocker $ Config.serverEnvVars config
    dockerContainerName = Config.dockerContainerName config
    dockerImageName = Config.dockerImageName config

    toEnvVarParams list =
      list >>= \(name, value) -> ["--env", name <> "=" <> value]
