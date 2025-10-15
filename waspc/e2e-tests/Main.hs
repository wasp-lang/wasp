import ContainerTest (runContainerTest)
import SnapshotTest (runSnapshotTest)
import System.Info (os)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Tests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import Tests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.WaspCompletionContainerTest (waspCompletionContainerTest)
import Tests.WaspInstallContainerTest (waspInstallContainerTest)
import Tests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.WaspNewSnapshotTest (waspNewSnapshotTest)
import Tests.WaspTelemetryContainerTest (waspTelemetryContainerTest)
import Tests.WaspUninstallContainerTest (waspUninstallContainerTest)

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
  containerTests <-
    if os == "linux"
      then
        mapM
          runContainerTest
          [ waspInstallContainerTest,
            waspUninstallContainerTest,
            waspTelemetryContainerTest,
            waspCompletionContainerTest
          ]
      else return []

  return $
    testGroup
      "E2E tests"
      [ testGroup "Snapshot Tests" snapshotTests,
        testGroup "Container Tests" containerTests
      ]
