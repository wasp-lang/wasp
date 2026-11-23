module Tests.WaspBuildStartTest (waspBuildStartTest) where

import Control.Monad (void, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower)
import E2E
import E2E.TestM (getWorkingDir)
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
          waspFails ["build", "start"] >>= assertOutputContains "You can build the app with the `wasp build` command.",
      -- `wasp build start` builds and runs the server's Docker image.
      test "succeed-built-project" $ do
        skipIfDockerDisabled
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          wasp ["build"]
          registerCleanupStep removeServerImageAndContainer
          ports <- allocateAppPorts
          app <- startInBackground $ waspCli (["build", "start", "-s", databaseUrlEnvVarName ++ "=none"] ++ appPortArgs ports)
          waitUntilListening app (appPortList ports) (15 * 60)
          interruptProcessGroupAndWaitUntilStopped app (appPortList ports)
    ]

-- | Wasp removes the server's container when it stops (`docker run --rm`), but
-- not if the test kills it first. It never removes the image.
--
-- Wasp names them after the project's path, so a leftover container would
-- make the next run of this test fail.
removeServerImageAndContainer :: TestM ()
removeServerImageAndContainer = do
  appUniqueId <- getAppUniqueId
  -- Lowercase and with these suffixes, like in `Wasp.Cli.Command.BuildStart.Config`.
  let imageName = map toLower $ appUniqueId ++ "-server"
      containerName = map toLower $ appUniqueId ++ "-server-container"
  void . runAllowingFailure $ command "docker" ["rm", "--force", containerName]
  void . runAllowingFailure $ command "docker" ["image", "rm", "--force", imageName]
  where
    getAppUniqueId = do
      -- Wasp finds the project dir through the OS's current dir, which has no
      -- symlinks.
      projectDir <- getWorkingDir >>= liftIO . (canonicalizePath >=> SP.parseAbsDir)
      appName <- either (failTest . ("Couldn't get the app name of the project: " ++)) (return . show) $ parseWaspProjectNameIntoAppName waspAppDirName
      return $ makeAppUniqueId projectDir appName
