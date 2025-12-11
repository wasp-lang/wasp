import SnapshotTest (runSnapshotTest)
import SnapshotTest.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest)
import SnapshotTest.WaspBuildSnapshotTest (waspBuildSnapshotTest)
import SnapshotTest.WaspCompileSnapshotTest (waspCompileSnapshotTest)
import SnapshotTest.WaspMigrateSnapshotTest (waspMigrateSnapshotTest)
import SnapshotTest.WaspNewSnapshotTest (waspNewSnapshotTest)
import System.Info (os)
import Test.Tasty (TestTree, defaultMain, testGroup)

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
