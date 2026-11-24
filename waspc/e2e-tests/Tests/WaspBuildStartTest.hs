module Tests.WaspBuildStartTest (waspBuildStartTest) where

import Data.Char (toLower)
import qualified Data.Text as T
import E2E
import Wasp.Cli.Command.CreateNewProject.AvailableTemplates (minimalStarterTemplate)
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
          -- If the check regresses, it builds and runs the server image.
          runExpectingFailure (waspCli ["build", "start"]) {timeoutSeconds = failFastTimeoutSeconds}
            >>= assertOutputContains "You can build the app with the `wasp build` command.",
      -- `wasp build start` builds and runs the server's Docker image.
      dockerTest "succeed-built-project" $
        withNewWaspProject minimalStarterTemplate $ do
          setDbToPostgres
          wasp ["build"]
          serverDockerNames <- getServerDockerNames
          registerCleanupStep $ removeServerImageAndContainer serverDockerNames
          ports <- allocateAppPorts
          app <- startInBackground $ waspCli (["build", "start", "-s", databaseUrlEnvVarName ++ "=none"] ++ appPortArgs ports)
          waitUntilListening app (appPortList ports) buildStartTimeoutSeconds
          assertServerRunsInContainer serverDockerNames
          interruptProcessGroupAndWaitUntilStopped app (appPortList ports)
    ]

-- | How long `wasp build start` may take to listen on its ports: it builds the
-- server's Docker image first.
buildStartTimeoutSeconds :: Int
buildStartTimeoutSeconds = 15 * 60

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
  (projectDir, appName) <- getProjectDirAndAppName
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
  docker ["container", "inspect", "--format", "{{.Config.Image}}", serverDockerNames.containerName]
    >>= assertStdoutContains (T.pack serverDockerNames.imageName)

-- | Wasp removes the server's container when it stops (`docker run --rm`), but
-- not if the test kills it first. It never removes the image.
removeServerImageAndContainer :: ServerDockerNames -> TestM ()
removeServerImageAndContainer serverDockerNames = do
  dockerCleanup ["rm", "--force", serverDockerNames.containerName]
  dockerCleanup ["image", "rm", "--force", serverDockerNames.imageName]
