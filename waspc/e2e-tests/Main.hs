import GoldenTest (runGoldenTest)
import GoldenTests.KitchenSinkGoldenTest (kitchenSinkGoldenTest)
import GoldenTests.WaspBuildGoldenTest (waspBuildGoldenTest)
import GoldenTests.WaspCompileGoldenTest (waspCompileGoldenTest)
import GoldenTests.WaspMigrateGoldenTest (waspMigrateGoldenTest)
import GoldenTests.WaspNewGoldenTest (waspNewGoldenTest)
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
  testGroup "Golden Tests"
    <$> mapM
      runGoldenTest
      [ waspNewGoldenTest,
        waspCompileGoldenTest,
        waspBuildGoldenTest,
        waspMigrateGoldenTest,
        kitchenSinkGoldenTest
      ]
