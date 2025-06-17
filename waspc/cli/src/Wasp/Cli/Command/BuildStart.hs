module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan (newChan)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Function ((&))
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Process (proc)
import System.Random (Random (randoms), RandomGen, newStdGen)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (BuildDirExists (BuildDirExists), InWaspProject (InWaspProject))
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Except (JobExcept, toJobExcept)
import qualified Wasp.Job.Except as JobExcept
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv, runProcessAsJob)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir)
import Wasp.Project.Env (dotEnvServer)

clientPort :: Integer
clientPort = 3000

serverPort :: Integer
serverPort = 3001

clientUrl :: String
clientUrl = "http://localhost:" ++ show clientPort

serverUrl :: String
serverUrl = "http://localhost:" ++ show serverPort

buildStart :: Command ()
buildStart = do
  InWaspProject waspProjectDir <- require
  BuildDirExists <- require

  -- TODO: Find a way to easily check we can connect to the DB.
  -- Right now we just assume it is running and let Prisma fail if it is not. It
  -- is not easy for us to do the same check as in other commands
  -- (`DBConnecionEstablished <- require`), because that needs a built Prisma
  -- schema, and currently we do that inside the Dockerfile. We might not have
  -- access to one in the `.wasp/build`, only in `.wasp/out` (which is not built
  -- for this command).

  -- TODO: Correct Wasp app name
  -- TODO: Check app runner, check we do the same things

  result <- liftIO $ runExceptT $ buildAndStartEverything waspProjectDir "wasp-app-name"

  case result of
    Left err -> cliSendMessageC $ Msg.Failure "Build and start failed" err
    Right () -> cliSendMessageC $ Msg.Success "Build and start completed successfully."

buildAndStartEverything :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> ExceptT String IO ()
buildAndStartEverything waspProjectDir dockerImageName =
  do
    liftIO $ cliSendMessage $ Msg.Start "Preparing client..."
    runAndPrintJob $ buildClient buildDir
    liftIO $ cliSendMessage $ Msg.Success "Client prepared."

    liftIO $ cliSendMessage $ Msg.Start "Preparing server..."
    runAndPrintJob $ buildServer buildDir dockerImageName
    liftIO $ cliSendMessage $ Msg.Success "Server prepared."

    liftIO $ cliSendMessage $ Msg.Start "Starting client and server..."
    runAndPrintJob $
      JobExcept.race_
        (startClient buildDir)
        (startServer waspProjectDir dockerImageName)
  where
    buildDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

    runAndPrintJob jobExcept = ExceptT $ do
      chan <- newChan
      (_, result) <-
        concurrently
          (readJobMessagesAndPrintThemPrefixed chan)
          (runExceptT $ jobExcept chan)
      return result

buildClient :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> JobExcept
buildClient buildDir =
  runNodeCommandAsJobWithExtraEnv
    [("REACT_APP_API_URL", serverUrl)]
    webAppDir
    "npm"
    ["run", "build"]
    J.WebApp
    & toJobExcept (("Building the client failed with exit code: " <>) . show)
  where
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir

startClient :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> JobExcept
startClient buildDir =
  runNodeCommandAsJob
    webAppDir
    "npm"
    [ "exec",
      "vite",
      "--",
      "preview", -- `preview` launches vite just as a webserver to the built files
      "--port",
      show clientPort,
      "--strictPort" -- This will make it fail if the port is already in use
    ]
    J.WebApp
    & toJobExcept (("Serving the client failed with exit code: " <>) . show)
  where
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir

buildServer :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> String -> JobExcept
buildServer buildDir dockerImageName =
  runProcessAsJob
    (proc "docker" ["build", "--tag", dockerImageName, SP.fromAbsDir buildDir])
    J.Server
    & toJobExcept (("Building the server failed with exit code: " <>) . show)

startServer :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> JobExcept
startServer projectDir dockerImageName =
  ( \chan -> do
      jwtSecret <- randomAsciiAlphaNum 32 <$> newStdGen

      runProcessAsJob
        ( proc "docker" $
            ["run", "--rm", "--env-file", envFilePath, "--network", "host"]
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
    & toJobExcept (("Running the server failed with exit code: " <>) . show)
  where
    envFilePath = SP.fromAbsFile $ projectDir </> dotEnvServer

    randomAsciiAlphaNum :: RandomGen g => Int -> g -> String
    randomAsciiAlphaNum len gen = take len $ filter isAlphaNum $ randoms gen
      where
        isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

    toDockerEnvFlags :: [(String, String)] -> [String]
    toDockerEnvFlags = concatMap (\(name, value) -> ["--env", name ++ "=" ++ value])
