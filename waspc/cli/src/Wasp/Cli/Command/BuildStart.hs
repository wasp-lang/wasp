module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent (Chan, newChan)
import Control.Concurrent.Async (race)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (proc)
import System.Random (Random (randoms), RandomGen, newStdGen)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Build (build)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv, runProcessAsJob)
import Wasp.Project.Common (buildDirInDotWaspDir, dotWaspDirInWaspProjectDir)

clientPort :: Integer
clientPort = 3000

serverPort :: Integer
serverPort = 3001

waspAppName :: String
waspAppName = "wasp-app-example"

clientUrl :: String
clientUrl = "http://localhost:" ++ show clientPort

serverUrl :: String
serverUrl = "http://localhost:" ++ show serverPort

postgresUrl :: String
postgresUrl = "postgresql://postgres:devpass1234@localhost:5433/" ++ waspAppName ++ "-db"

buildStart :: Command ()
buildStart = do
  InWaspProject waspProjectDir <- require

  chan <- liftIO newChan

  let buildDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

  result <-
    liftIO $
      race
        (buildAndStartClient buildDir chan)
        (buildAndStartServer buildDir chan)

  let printError = either error return
  either printError printError result

buildAndStartClient :: SP.Path SP.System SP.Abs (SP.Dir ProjectRootDir) -> Chan J.JobMessage -> IO (Either String ())
buildAndStartClient buildDir chan =
  handleExitCode (buildClientJob chan) (("Building the client failed with exit code: " ++) . show)
    `chain` handleExitCode (startClientJob chan) (("Serving the client failed with exit code: " ++) . show)
  where
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir

    buildClientJob = runNodeCommandAsJobWithExtraEnv [("REACT_APP_API_URL", serverUrl)] webAppDir "npm" ["run", "build"] J.WebApp

    startClientJob =
      runNodeCommandAsJob webAppDir "npm" ["exec", "vite", "preview", "--port", show clientPort] J.WebApp

buildAndStartServer :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> Chan J.JobMessage -> IO (Either String ())
buildAndStartServer buildDir chan = do
  jwtSecret <- randomSecret 32 <$> newStdGen
  handleExitCode (dockerBuild waspAppName chan) (("Building the server failed with exit code: " ++) . show)
    `chain` handleExitCode (dockerRun waspAppName jwtSecret chan) (("Running the server failed with exit code: " ++) . show)
  where
    -- Turns a list of pairs into command line flags.
    flags = concatMap (\(name, value) -> ["--" ++ name, value])
    -- Turns a list of pairs into environment variables for the Docker run command.
    envFlags = concatMap (\(name, value) -> ["--env", name ++ "=" ++ value])

    -- Generates a random alphanumeric secret of the specified length.
    -- Uses ASCII alphanumeric characters (uppercase, lowercase, and digits).
    randomSecret :: RandomGen g => Int -> g -> String
    randomSecret len gen =
      take len $ filter isAlphaNum $ randoms gen
      where
        isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

    dockerBuild name = runProcessAsJob (System.Process.proc "docker" ["build", "--tag", name, show buildDir]) J.Server

    dockerRun name jwtSecret =
      runProcessAsJob
        ( System.Process.proc
            "docker"
            ( ["run", "--rm"]
                ++ flags [("env-file", ".env.server"), ("network", "host")]
                ++ envFlags [("DATABASE_URL", postgresUrl), ("WASP_WEB_CLIENT_URL", clientUrl), ("WASP_SERVER_URL", serverUrl), ("JWT_SECRET", jwtSecret)]
                ++ [name]
            )
        )
        J.Server

-- Turns an IO ExitCode into an IO Either with an error message.
handleExitCode :: IO ExitCode -> (Int -> String) -> IO (Either String ())
handleExitCode action f = do
  fromExitCode f <$> action

-- Runs the second action only after the first one ends successfully.
chain :: IO (Either String ()) -> IO (Either String ()) -> IO (Either String ())
chain action1 action2 = do
  result1 <- action1
  case result1 of
    Left err -> pure $ Left err
    Right _ -> action2

-- Turns an ExitCode into an Either (failures are converted to a String).
fromExitCode :: (Int -> String) -> ExitCode -> Either String ()
fromExitCode _ ExitSuccess = Right ()
fromExitCode f (ExitFailure code) = Left $ f code
