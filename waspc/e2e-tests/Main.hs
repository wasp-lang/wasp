import SnapshotTest (runSnapshotTest)
import System.Info (os)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Tests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import Tests.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import Tests.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import Tests.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import Tests.WaspNewSnapshotTest (waspNewSnapshotTest)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else tests >>= defaultMain

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
