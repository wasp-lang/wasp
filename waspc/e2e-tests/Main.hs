import ContainerTest (runContainerTest)
import Data.Maybe (isJust)
import EphemeralTest (runEphemeralTest)
import SnapshotTest (runSnapshotTest)
import System.Environment (lookupEnv)
import System.Info (os)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Tests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import Tests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.WaspCompletionEphemeralTest (waspCompletionEphemeralTest)
import Tests.WaspDockerfileEphemeralTest (waspDockerfileEphemeralTest)
import Tests.WaspInfoEphemeralTest (waspInfoEphemeralTest)
import Tests.WaspInstallContainerTest (waspInstallContainerTest)
import Tests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.WaspNewSnapshotTest (waspNewSnapshotTest)
import Tests.WaspTelemetryEphemeralTest (waspTelemetryEphemeralTest)
import Tests.WaspUninstallContainerTest (waspUninstallContainerTest)
import Tests.WaspVersionEphemeralTest (waspVersionEphemeralTest)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else tests >>= defaultMain

-- TODO: Investigate automatically discovering the tests.
tests :: IO TestTree
tests = do
  snapshotTests <-
    mapM
      runSnapshotTest
      [ waspNewSnapshotTest,
        waspCompileSnapshotTest,
        waspBuildSnapshotTest,
        waspMigrateSnapshotTest,
        kitchenSinkSnapshotTest
      ]
  shouldSkipDocker <- isJust <$> lookupEnv "WASP_E2E_TESTS_SKIP_DOCKER"

  print $ "Is skip docker env var?: " ++ show shouldSkipDocker
  print $ "Is macOS?: " ++ show (os == "darwin")

  containerTests <-
    if shouldSkipDocker || (os == "darwin")
      then return []
      else
        mapM
          runContainerTest
          [ waspInstallContainerTest,
            waspUninstallContainerTest
          ]
  ephemeralTests <-
    mapM
      runEphemeralTest
      [ waspTelemetryEphemeralTest,
        waspCompletionEphemeralTest,
        waspInfoEphemeralTest,
        waspVersionEphemeralTest,
        waspDockerfileEphemeralTest
      ]

  return $
    testGroup
      "E2E tests"
      [ testGroup "Snapshot Tests" snapshotTests,
        testGroup "Container Tests" containerTests,
        testGroup "Ephemeral Tests" ephemeralTests
      ]
