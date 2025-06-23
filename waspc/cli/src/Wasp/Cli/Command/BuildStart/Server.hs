{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}
module Wasp.Cli.Command.BuildStart.Server
  ( buildServer,
    startServer,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Function ((&))
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Process (proc)
import System.Random (Random (randoms), RandomGen, newStdGen)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig)
import qualified Wasp.Cli.Command.BuildStart.Config as Config
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runProcessAsJob)
import Wasp.Project.Env (dotEnvServer)

buildServer :: BuildStartConfig -> ExceptJob
buildServer config =
  runProcessAsJob
    (proc "docker" ["build", "--tag", dockerImageName, SP.fromAbsDir buildDir])
    J.Server
    & toExceptJob (("Building the server failed with exit code: " <>) . show)
  where
    buildDir = Config.buildDir config
    dockerImageName = Config.dockerImageName config

startServer :: BuildStartConfig -> ExceptJob
startServer config =
  ( \chan -> do
      jwtSecret <- randomAsciiAlphaNum 32 <$> newStdGen

      runProcessAsJob
        ( proc "docker" $
            ["run", "--name", dockerContainerName, "--rm", "--env-file", envFilePath, "--network", "host"]
              ++
              -- We specifically pass this environment variable from the current
              -- execution to the server container because Prisma will need it,
              -- and it is not set in the .env file (wasp start complains if so).
              ["--env", "DATABASE_URL"]
              ++ toDockerEnvFlags
                [ ("WASP_WEB_CLIENT_URL", clientUrl),
                  ("WASP_SERVER_URL", serverUrl),
                  ("JWT_SECRET", jwtSecret)
                ]
              ++ [dockerImageName]
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

    randomAsciiAlphaNum :: RandomGen g => Int -> g -> String
    randomAsciiAlphaNum len gen = take len $ filter isAlphaNum $ randoms gen
      where
        isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

    toDockerEnvFlags :: [(String, String)] -> [String]
    toDockerEnvFlags = concatMap (\(name, value) -> ["--env", name ++ "=" ++ value])
