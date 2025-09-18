import GoldenTest (runGoldenTest)
import System.Info (os)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Tests.KitchenSinkGoldenTest (kitchenSinkGoldenTest)
import Tests.WaspBuildGoldenTest (waspBuildGoldenTest)
import Tests.WaspCompileGoldenTest (waspCompileGoldenTest)
import Tests.WaspMigrateGoldenTest (waspMigrateGoldenTest)
import Tests.WaspNewGoldenTest (waspNewGoldenTest)

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
