module Wasp.Cli.Command.BuildStart (buildStart) where

import Control.Concurrent (Chan, newChan)
import Control.Concurrent.Async (concurrently, race)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.Process (proc)
import System.Random (Random (randoms), RandomGen, newStdGen)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob, runNodeCommandAsJobWithExtraEnv, runProcessAsJob)
import Wasp.Project.Common (buildDirInDotWaspDir, dotWaspDirInWaspProjectDir)

clientPort :: Integer
clientPort = 3000

serverPort :: Integer
serverPort = 3001

clientUrl :: String
clientUrl = "http://localhost:" ++ show clientPort

serverUrl :: String
serverUrl = "http://localhost:" ++ show serverPort

postgresUrl :: String -> String
postgresUrl waspAppName = "postgresql://postgres:devpass1234@localhost:5433/" ++ waspAppName ++ "-db"

buildStart :: Command ()
buildStart = do
  InWaspProject waspProjectDir <- require

  chan <- liftIO newChan

  let buildDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

  void $
    liftIO $
      concurrently
        (readJobMessagesAndPrintThemPrefixed chan)
        ( buildClient buildDir chan
            >> buildServer buildDir "wasp-app-name" chan
        )

  void $
    liftIO $
      concurrently
        (readJobMessagesAndPrintThemPrefixed chan)
        ( race
            (startClient buildDir chan)
            (startServer "wasp-app-name" chan)
        )

buildClient :: SP.Path SP.System SP.Abs (SP.Dir ProjectRootDir) -> Chan J.JobMessage -> IO ()
buildClient buildDir chan =
  runNodeCommandAsJobWithExtraEnv [("REACT_APP_API_URL", serverUrl)] webAppDir "npm" ["run", "build"] J.WebApp chan
    >>= tagErrorExit (\code -> "Building the client failed with exit code: " ++ show code)
  where
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir

startClient :: SP.Path SP.System SP.Abs (SP.Dir ProjectRootDir) -> Chan J.JobMessage -> IO ()
startClient buildDir chan =
  runNodeCommandAsJob webAppDir "npm" ["exec", "vite", "preview", "--port", show clientPort] J.WebApp chan
    >>= tagErrorExit (\code -> "Serving the client failed with exit code: " ++ show code)
  where
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir

buildServer :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> String -> Chan J.JobMessage -> IO ()
buildServer buildDir waspAppName chan =
  runProcessAsJob (System.Process.proc "docker" (["build"] ++ flags [("tag", waspAppName)] ++ [SP.fromAbsDir buildDir])) J.Server chan
    >>= tagErrorExit (\code -> "Building the server failed with exit code: " ++ show code)

startServer :: String -> Chan J.JobMessage -> IO ()
startServer waspAppName chan =
  do
    jwtSecret <- randomSecret 32 <$> newStdGen
    runProcessAsJob
      ( proc
          "docker"
          ( ["run", "--rm"]
              ++ flags
                [ ("env-file", ".env.server"),
                  ("network", "host")
                ]
              ++ envFlags
                [ ("DATABASE_URL", postgresUrl waspAppName),
                  ("WASP_WEB_CLIENT_URL", clientUrl),
                  ("WASP_SERVER_URL", serverUrl),
                  ("JWT_SECRET", jwtSecret)
                ]
              ++ [waspAppName]
          )
      )
      J.Server
      chan
      >>= tagErrorExit (\code -> "Running the server failed with exit code: " ++ show code)
  where
    -- Generates a random alphanumeric secret of the specified length.
    -- Uses ASCII alphanumeric characters (uppercase, lowercase, and digits).
    randomSecret :: RandomGen g => Int -> g -> String
    randomSecret len gen =
      take len $ filter isAlphaNum $ randoms gen
      where
        isAlphaNum c = isAsciiUpper c || isAsciiLower c || isDigit c

-- Turns a list of pairs into command line flags.
flags :: [(String, String)] -> [String]
flags = concatMap (\(name, value) -> ["--" ++ name, value])

-- Turns a list of pairs into environment variables for the Docker run command.
envFlags :: [(String, String)] -> [String]
envFlags = concatMap (\(name, value) -> ["--env", name ++ "=" ++ value])

tagErrorExit :: (Int -> String) -> ExitCode -> IO ()
tagErrorExit f exitCode =
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure code -> error $ f code
