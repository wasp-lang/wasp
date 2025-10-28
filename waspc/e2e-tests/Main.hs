import ContainerTest (runContainerTest)
import ContainerTest.WaspInstallContainerTest (waspInstallContainerTest)
import ContainerTest.WaspUninstallContainerTest (waspUninstallContainerTest)
import Data.Maybe (isJust)
import EphemeralTest (runEphemeralTest)
import EphemeralTest.WaspCompletionEphemeralTest (waspCompletionEphemeralTest)
import EphemeralTest.WaspDockerfileEphemeralTest (waspDockerfileEphemeralTest)
import EphemeralTest.WaspInfoEphemeralTest (waspInfoEphemeralTest)
import EphemeralTest.WaspTelemetryEphemeralTest (waspTelemetryEphemeralTest)
import EphemeralTest.WaspVersionEphemeralTest (waspVersionEphemeralTest)
import SnapshotTest (runSnapshotTest)
import SnapshotTest.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import SnapshotTest.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import SnapshotTest.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import SnapshotTest.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import SnapshotTest.WaspNewSnapshotTest (waspNewSnapshotTest)
import System.Environment (lookupEnv)
import System.Info (os)
import Test.Tasty (TestTree, defaultMain, testGroup)

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
      else do
        mapM
          runContainerTest
          =<< sequence
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
