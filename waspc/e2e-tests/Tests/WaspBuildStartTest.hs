module Tests.WaspBuildStartTest (waspBuildStartTest) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import qualified Data.Text as T
import E2E
import qualified StrongPath as SP
import System.Directory (canonicalizePath)
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (parseWaspProjectNameIntoAppName)
import Wasp.Project.Common (makeAppUniqueId)
import Wasp.Project.Db (databaseUrlEnvVarName)

waspBuildStartTest :: E2ETest
waspBuildStartTest =
  testGroup
    "wasp-build-start"
    [ test "fail-outside-project" $ waspFailsOutsideProject ["build", "start"],
      test "fail-unbuilt-project" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          -- If the check regresses, we don't want to wait for the server
          -- image to be built and run until the default timeout.
          runExpectingFailure (waspCli ["build", "start"]) {timeoutSeconds = 2 * 60}
            >>= assertOutputContains "You can build the app with the `wasp build` command.",
      -- `wasp build start` builds and runs the server's Docker image.
      test "succeed-built-project" $ do
        skipIfDockerDisabled
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          wasp ["build"]
          serverDockerNames <- getServerDockerNames
          registerCleanupStep $ removeServerImageAndContainer serverDockerNames
          ports <- allocateAppPorts
          app <- startInBackground $ waspCli (["build", "start", "-s", databaseUrlEnvVarName ++ "=none"] ++ appPortArgs ports)
          waitUntilListening app (appPortList ports) (15 * 60)
          assertServerRunsInContainer serverDockerNames
          interruptProcessGroupAndWaitUntilStopped app (appPortList ports)
    ]

-- | The names of the Docker image and container that `wasp build start` runs
-- the server in.
data ServerDockerNames = ServerDockerNames
  { imageName :: String,
    containerName :: String
  }

-- | Wasp names them after the project's path, so a leftover container would
-- make the next run of this test fail. Lowercase and with these suffixes,
-- like in `Wasp.Cli.Command.BuildStart.Config`.
getServerDockerNames :: TestM ServerDockerNames
getServerDockerNames = do
  workingDir <- getWorkingDir
  -- Wasp finds the project dir through the OS's current dir, which has no
  -- symlinks.
  projectDir <- liftIO $ canonicalizePath workingDir >>= SP.parseAbsDir
  appName <- case parseWaspProjectNameIntoAppName waspAppDirName of
    Left err -> failTest $ "Couldn't get the app name of the project: " ++ err
    Right name -> return $ show name
  let appUniqueId = makeAppUniqueId projectDir appName
  return
    ServerDockerNames
      { imageName = map toLower $ appUniqueId ++ "-server",
        containerName = map toLower $ appUniqueId ++ "-server-container"
      }

-- | Also proves that we derive the names right, since the cleanup can't tell
-- a wrong name from a container or image that's already gone.
assertServerRunsInContainer :: ServerDockerNames -> TestM ()
assertServerRunsInContainer serverDockerNames =
  run (command "docker" ["container", "inspect", "--format", "{{.Config.Image}}", serverDockerNames.containerName])
    >>= assertStdoutContains (T.pack serverDockerNames.imageName)

-- | Wasp removes the server's container when it stops (`docker run --rm`), but
-- not if the test kills it first. It never removes the image.
removeServerImageAndContainer :: ServerDockerNames -> TestM ()
removeServerImageAndContainer serverDockerNames = do
  dockerInCleanup ["rm", "--force", serverDockerNames.containerName]
  void $ dockerInCleanup ["image", "rm", "--force", serverDockerNames.imageName]
  where
    dockerInCleanup arguments = runAllowingFailure (command "docker" arguments) {timeoutSeconds = cleanupCommandTimeoutSeconds}
