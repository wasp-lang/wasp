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
import Wasp.Cli.Command.Require (DbConnectionEstablished (DbConnectionEstablished), InBuildDir (InBuildDir), InWaspProject (InWaspProject))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Except (JobExcept, toJobExcept)
import qualified Wasp.Job.Except as JobExcept
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv, runProcessAsJob)
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

postgresUrl :: String
postgresUrl = "postgresql://postgresWaspDevUser:postgresWaspDevPass@localhost:5432/todoApp-c032198002"

buildStart :: Command ()
buildStart = do
  InWaspProject waspProjectDir <- require
  InBuildDir DbConnectionEstablished <- require

  -- TODO: Nice error when the build directory does not exist.
  -- TODO: Progress messages with emojis and such
  -- TODO: How to handle errors well?
  -- TODO: Correct Wasp app name
  -- TODO: Correct DB name
  -- TODO: Check app runner, check we do the same things

  liftIO $ do
    result <- runExceptT $ buildAndStartEverything waspProjectDir "wasp-app-name"

    case result of
      Left err -> putStrLn $ "\nBuild and start failed: " ++ err
      Right () -> do
        putStrLn "\nBuild and start succeeded!"

buildAndStartEverything :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> ExceptT String IO ()
buildAndStartEverything waspProjectDir dockerImageName =
  do
    runAndPrintJob $ buildClient buildDir
    runAndPrintJob $ buildServer buildDir dockerImageName
    runAndPrintJob $
      JobExcept.concurrently
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
    [("REACT_APP_API_URL", serverUrl)] -- Give the client the URL to the server
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
      jwtSecret <- randomAsciiAlpaNum 32 <$> newStdGen

      runProcessAsJob
        ( proc "docker" $
            ["run", "--rm", "--env-file", envFilePath, "--network", "host"]
              ++ toDockerEnvFlags
                [ ("DATABASE_URL", postgresUrl),
                  ("WASP_WEB_CLIENT_URL", clientUrl),
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

    randomAsciiAlpaNum :: RandomGen g => Int -> g -> String
    randomAsciiAlpaNum len gen = take len $ filter isAlphaNum $ randoms gen
      where
        isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

    -- Turns a list of string pairs into "--env" arguments for the Docker run command.
    toDockerEnvFlags :: [(String, String)] -> [String]
    toDockerEnvFlags = concatMap (\(name, value) -> ["--env", name ++ "=" ++ value])
