import GoldenTest (runGoldenTest)
import GoldenTests.KitchenSink (kitchenSinkGoldenTest)
import GoldenTests.MinimalStarter (minimalStarterGoldenTest)
import GoldenTests.WaspMigrate (waspMigrateGoldenTest)
import System.Info (os)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = do
  if os == "mingw32"
    then putStrLn "Skipping end-to-end tests on Windows due to tests using *nix-only commands"
    else tests >>= defaultMain

-- TODO: Add more tests to simulate full Todo app tutorial.
--       Some of this requires waspStart and stdout parsing.
-- TODO: Investigate automatically discovering the tests.
tests :: IO TestTree
tests =
  testGroup "Golden Tests"
    <$> mapM
      runGoldenTest
      [ minimalStarterGoldenTest,
        waspMigrateGoldenTest,
        kitchenSinkGoldenTest
      ]
