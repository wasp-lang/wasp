module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently_)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import StrongPath ((</>))
import qualified StrongPath as SP
import System.Process (proc)
import System.Random (Random (randoms), RandomGen, newStdGen)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Job as J
import Wasp.Job.Except as JE
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

  -- TODO: Nice error when the build directory does not exist.
  -- TODO: Nice error when DB is not running.
  -- TODO: Progress messages with emojis and such
  -- TODO: How to handle errors well?
  -- TODO: Correct Wasp app name
  -- TODO: Correct DB name
  -- TODO: Check app runner, check we do the same things
  -- TODO: Print URL location

  -- FIXME: IT SEEMS IT DOESN'T WORK

  void $
    liftIO $ do
      chan <- newChan
      concurrently_
        (readJobMessagesAndPrintThemPrefixed chan)
        (JE.run chan $ buildAndStartAllJob waspProjectDir "wasp-app")

buildAndStartAllJob :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> JobExcept
buildAndStartAllJob waspProjectDir imageName = do
  let buildDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir

  void $ buildClientJob buildDir
  void $ buildServerJob buildDir imageName

  JE.concurrent
    [ startClientJob buildDir,
      startServerJob waspProjectDir imageName
    ]

buildClientJob :: SP.Path SP.System SP.Abs (SP.Dir ProjectRootDir) -> JobExcept
buildClientJob buildDir =
  fromJob (("Building the client failed with exit code: " <>) . show) $
    runNodeCommandAsJobWithExtraEnv [("REACT_APP_API_URL", serverUrl)] webAppDir "npm" ["run", "build"] J.WebApp
  where
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir

startClientJob :: SP.Path SP.System SP.Abs (SP.Dir ProjectRootDir) -> JobExcept
startClientJob buildDir =
  fromJob (("Serving the client failed with exit code: " <>) . show) $
    runNodeCommandAsJob webAppDir "npm" ["exec", "vite", "preview", "--port", show clientPort] J.WebApp
  where
    webAppDir = buildDir </> Common.webAppRootDirInProjectRootDir

buildServerJob :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> String -> JobExcept
buildServerJob buildDir waspAppName =
  JE.fromJob (("Building the server failed with exit code: " <>) . show) $
    runProcessAsJob
      ( proc "docker" $
          ["build"]
            ++ flags [("tag", waspAppName)]
            ++ [SP.fromAbsDir buildDir]
      )
      J.Server

startServerJob :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> String -> JobExcept
startServerJob projectDir waspAppName =
  JE.fromJob
    (("Running the server failed with exit code: " <>) . show)
    $ \chan -> do
      jwtSecret <- randomSecret 32 <$> newStdGen
      runProcessAsJob
        ( proc "docker" $
            ["run", "--rm"]
              ++ flags [("env-file", envFilePath), ("network", "host")]
              ++ envFlags
                [ ("DATABASE_URL", postgresUrl),
                  ("WASP_WEB_CLIENT_URL", clientUrl),
                  ("WASP_SERVER_URL", serverUrl),
                  ("JWT_SECRET", jwtSecret)
                ]
              ++ [waspAppName]
        )
        J.Server
        chan
  where
    envFilePath = SP.fromAbsFile $ projectDir </> dotEnvServer

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
