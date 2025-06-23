{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}
module Wasp.Cli.Command.BuildStart.Server
  ( buildServer,
    startServer,
    makeAppDockerImageName,
    makeAppDockerContainerName,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.Function ((&))
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Process (proc)
import System.Random (Random (randoms), RandomGen, newStdGen)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (defaultDevServerUrl)
import qualified Wasp.Job as J
import Wasp.Job.Except (ExceptJob, toExceptJob)
import Wasp.Job.Process (runProcessAsJob)
import Wasp.Project.Common (WaspProjectDir, makeAppUniqueId)
import Wasp.Project.Env (dotEnvServer)

buildServer :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> String -> ExceptJob
buildServer buildDir dockerImageName =
  runProcessAsJob
    (proc "docker" ["build", "--tag", dockerImageName, SP.fromAbsDir buildDir])
    J.Server
    & toExceptJob (("Building the server failed with exit code: " <>) . show)

startServer :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> String -> String -> ExceptJob
startServer projectDir clientUrl dockerImageName dockerContainerName =
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
                  ("WASP_SERVER_URL", defaultDevServerUrl),
                  ("JWT_SECRET", jwtSecret)
                ]
              ++ [dockerImageName]
        )
        J.Server
        chan
  )
    & toExceptJob (("Running the server failed with exit code: " <>) . show)
  where
    envFilePath = SP.fromAbsFile $ projectDir </> dotEnvServer

    randomAsciiAlphaNum :: RandomGen g => Int -> g -> String
    randomAsciiAlphaNum len gen = take len $ filter isAlphaNum $ randoms gen
      where
        isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

    toDockerEnvFlags :: [(String, String)] -> [String]
    toDockerEnvFlags = concatMap (\(name, value) -> ["--env", name ++ "=" ++ value])

-- | Docker image name unique for the Wasp project with specified path and name.
makeAppDockerImageName :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> String
makeAppDockerImageName waspProjectDir appName =
  map toLower $
    makeAppUniqueId waspProjectDir appName <> "-server"

makeAppDockerContainerName :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> String
makeAppDockerContainerName waspProjectDir appName =
  map toLower $
    makeAppUniqueId waspProjectDir appName <> "-server-container"
