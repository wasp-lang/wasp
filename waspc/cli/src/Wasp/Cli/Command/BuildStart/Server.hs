{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}
module Wasp.Cli.Command.BuildStart.Server
  ( buildServer,
    startServer,
  )
where

import Data.Function ((&))
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Process (proc)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runProcessAsJob)
import Wasp.Project.Env (dotEnvServer)
import Wasp.Util.Random (genRandomAsciiAlphaNumsIO)

buildServer :: BuildStartConfig -> ExceptJob
buildServer config =
  runProcessAsJob
    (proc "docker" ["build", "--tag", dockerImageName, contextPath])
    J.Server
    & toExceptJob (("Building the server failed with exit code: " <>) . show)
  where
    buildDir = Config.buildDir config
    contextPath = SP.fromAbsDir buildDir -- The folder with the Dockerfile and the files it needs to build.
    dockerImageName = Config.dockerImageName config

startServer :: BuildStartConfig -> ExceptJob
startServer config =
  ( \chan -> do
      jwtSecret <- genRandomAsciiAlphaNumsIO 32

      runProcessAsJob
        ( proc
            "docker"
            [ "run",
              "--name",
              dockerContainerName,
              "--rm",
              "--env-file",
              envFilePath,
              "--network",
              "host",
              "--env",
              "WASP_WEB_CLIENT_URL=" <> clientUrl,
              "--env",
              "WASP_SERVER_URL=" <> serverUrl,
              "--env",
              "JWT_SECRET=" <> jwtSecret,
              -- We specifically pass `DATABASE_URL` from the current
              -- execution to the server container because Prisma will need
              -- it, and it is not set in the .env file (wasp start
              -- complains if so).
              "--env",
              "DATABASE_URL",
              dockerImageName
            ]
        )
        J.Server
        chan
  )
    & toExceptJob (("Running the server failed with exit code: " <>) . show)
  where
    projectDir = Config.projectDir config
    envFilePath = SP.fromAbsFile $ projectDir </> dotEnvServer

    clientUrl = Config.clientUrl config
    serverUrl = Config.serverUrl config
    dockerContainerName = Config.dockerContainerName config
    dockerImageName = Config.dockerImageName config
