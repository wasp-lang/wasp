module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan (newChan)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.Function ((&))
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Process (proc)
import System.Random (Random (randoms), RandomGen, newStdGen)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (BuildDirExists (BuildDirExists), InWaspProject (InWaspProject))
import Wasp.Cli.Message (cliSendMessage)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.ServerGenerator.Common (defaultDevServerUrl)
import Wasp.Generator.WebAppGenerator.Common (defaultClientPort, getDefaultDevClientUrl)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Except (JobExcept, toJobExcept)
import qualified Wasp.Job.Except as JobExcept
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv, runProcessAsJob)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, makeAppUniqueId)
import Wasp.Project.Env (dotEnvServer)

buildStart :: Command ()
buildStart = do
  InWaspProject waspProjectDir <- require
  appSpec <- analyze waspProjectDir
  BuildDirExists <- require

  -- TODO: Find a way to easily check we can connect to the DB.
  -- Right now we just assume it is running and let Prisma fail if it is not. It
  -- is not easy for us to do the same check as in other commands
  -- (`DBConnecionEstablished <- require`), because that needs a built Prisma
  -- schema, and currently we do that inside the Dockerfile, not in the
  -- `.wasp/build` folder, like the `wasp start` command does.
  -- It is not a big problem right now, because Prisma will fail shortly after
  -- the server starts if the DB is not running anyway, and with a very clear
  -- error message that we print.

  let (appName, _) = ASV.getApp appSpec
  let imageName = makeAppImageName waspProjectDir appName
  let containerName = makeAppContainerName waspProjectDir appName

  result <-
    liftIO $
      runExceptT $
        buildAndStartEverything waspProjectDir appSpec imageName containerName

  case result of
    Left err -> cliSendMessageC $ Msg.Failure "Build and start failed" err
    Right () -> cliSendMessageC $ Msg.Success "Build and start completed successfully."

buildAndStartEverything :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> AppSpec -> String -> String -> ExceptT String IO ()
buildAndStartEverything waspProjectDir appSpec dockerImageName dockerContainerName =
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
        (startServer waspProjectDir clientUrl dockerImageName dockerContainerName)
  where
    buildDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

    clientUrl = getDefaultDevClientUrl appSpec

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
    [("REACT_APP_API_URL", defaultDevServerUrl)]
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
      show defaultClientPort,
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

startServer :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> String -> String -> JobExcept
startServer projectDir clientUrl dockerImageName containerName =
  ( \chan -> do
      jwtSecret <- randomAsciiAlphaNum 32 <$> newStdGen

      runProcessAsJob
        ( proc "docker" $
            ["run", "--name", containerName, "--rm", "--env-file", envFilePath, "--network", "host"]
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
    & toJobExcept (("Running the server failed with exit code: " <>) . show)
  where
    envFilePath = SP.fromAbsFile $ projectDir </> dotEnvServer

    randomAsciiAlphaNum :: RandomGen g => Int -> g -> String
    randomAsciiAlphaNum len gen = take len $ filter isAlphaNum $ randoms gen
      where
        isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

    toDockerEnvFlags :: [(String, String)] -> [String]
    toDockerEnvFlags = concatMap (\(name, value) -> ["--env", name ++ "=" ++ value])

-- | Docker image name unique for the Wasp project with specified path and name.
makeAppImageName :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> String
makeAppImageName waspProjectDir appName =
  map toLower $
    makeAppUniqueId waspProjectDir appName <> "-server"

makeAppContainerName :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> String
makeAppContainerName waspProjectDir appName =
  map toLower $
    makeAppUniqueId waspProjectDir appName <> "-server-container"
