import SnapshotTest (runSnapshotTest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Tests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import Tests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.WaspNewSnapshotTest (waspNewSnapshotTest)

main :: IO ()
main = tests >>= defaultMain

-- TODO: Investigate automatically discovering the tests.
tests :: IO TestTree
tests =
  testGroup "Snapshot Tests"
    <$> mapM
      runSnapshotTest
      [ waspNewSnapshotTest,
        waspCompileSnapshotTest,
        waspBuildSnapshotTest,
        waspMigrateSnapshotTest,
        kitchenSinkSnapshotTest
      ]
