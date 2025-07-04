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
import Wasp.Generator.ServerGenerator.AuthG (jwtSecretEnvVarName)
import Wasp.Generator.ServerGenerator.Common (clientUrlFromServerEnvVarName, serverUrlFromServerEnvVarName)
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runProcessAsJob)
import Wasp.Project.Db (databaseUrlEnvVarName)
import Wasp.Project.Env (dotEnvServer)
import Wasp.Util.Random (genRandomAsciiAlphaNumsIO)

buildServer :: BuildStartConfig -> ExceptJob
buildServer config =
  runProcessAsJob
    (proc "docker" ["build", "--tag", dockerImageName, dockerContextDir])
    J.Server
    & toExceptJob (("Building the server failed with exit code: " <>) . show)
  where
    buildDir = Config.buildDir config
    dockerContextDir = SP.fromAbsDir buildDir
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
              clientUrlFromServerEnvVarName <> "=" <> clientUrl,
              "--env",
              serverUrlFromServerEnvVarName <> "=" <> serverUrl,
              "--env",
              jwtSecretEnvVarName <> "=" <> jwtSecret,
              -- We specifically pass `DATABASE_URL` from the current execution
              -- to the server container because Prisma will need it, and it is
              -- not set in the .env file (wasp start complains if so). We pass
              -- it without a value so that it is inherited from the current
              -- environment.
              "--env",
              databaseUrlEnvVarName,
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

    (_, clientUrl) = Config.clientPortAndUrl config
    serverUrl = Config.serverUrl config
    dockerContainerName = Config.dockerContainerName config
    dockerImageName = Config.dockerImageName config
