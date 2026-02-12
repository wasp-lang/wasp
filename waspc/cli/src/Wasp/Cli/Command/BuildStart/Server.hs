module Wasp.Cli.Command.BuildStart.Server
  ( buildServer,
    startServer,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.List (isInfixOf)
import qualified StrongPath as SP
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (proc, readProcessWithExitCode)
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
startServer config chan = do
  removeStaleContainerIfExists
  toExceptJob (("Running the server failed with exit code: " <>) . show) dockerRunJob chan
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
    dockerRunJob =
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

    toEnvVarParams list =
      list >>= \(name, value) -> ["--env", name <> "=" <> value]

    removeStaleContainerIfExists = do
      existingContainer <- liftIO isContainerPresent
      case existingContainer of
        Left err -> throwError err
        Right isPresent -> when isPresent $ do
          removeExitCode <- liftIO $ runProcessAsJob (proc "docker" ["rm", "-f", dockerContainerName]) J.Server chan
          case removeExitCode of
            ExitSuccess -> pure ()
            ExitFailure code ->
              throwError $
                "Removing stale server container failed with exit code: " <> show code

    isContainerPresent = do
      let args =
            [ "container",
              "ls",
              "--all",
              "--filter",
              "name=^/" <> dockerContainerName <> "$",
              "--format",
              "{{.Names}}"
            ]
      (exitCode, output, stderr) <- readProcessWithExitCode "docker" args ""
      case exitCode of
        ExitSuccess -> return $ Right $ dockerContainerName `elem` lines output
        ExitFailure code ->
          return $
            Left $
              "Checking for an existing server container failed with exit code: "
                <> show code
                <> if null stderr then "" else " (" <> stderr <> ")"
