import EphemeralTest (runEphemeralTest)
import EphemeralTest.WaspCompletionEphemeralTest (waspCompletionEphemeralTest)
import EphemeralTest.WaspDepsEphemeralTest (waspDepsEphemeralTest)
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
import System.Info (os)
import Test.Tasty (TestTree, defaultMain, testGroup)
import EphemeralTest.WaspDbStartEphemeralTest (waspDbStartEphemeralTest)
import EphemeralTest.WaspDbResetEphemeralTest (waspDbResetEphemeralTest)

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
  ephemeralTests <-
    mapM
      runEphemeralTest
      [ waspTelemetryEphemeralTest,
        waspCompletionEphemeralTest,
        waspInfoEphemeralTest,
        waspVersionEphemeralTest,
        waspDockerfileEphemeralTest,
        waspDepsEphemeralTest,
        waspDbStartEphemeralTest,
        waspDbResetEphemeralTest
      ]

  return $
    testGroup
      "E2E tests"
      [ testGroup "Snapshot Tests" snapshotTests,
        testGroup "Ephemeral Tests" ephemeralTests
      ]
