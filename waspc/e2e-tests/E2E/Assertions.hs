module E2E.Assertions
  ( assertTrue,
    assertEqual,
    assertOutputContains,
    assertDirectoryExists,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import E2E.Process (ProcessResult (..))
import E2E.TestM (TestM, failTest, resolvePath)
import System.Directory (doesDirectoryExist)

assertTrue :: String -> Bool -> TestM ()
assertTrue description condition = unless condition $ failTest $ "Assertion failed: " ++ description

assertEqual :: (Eq a, Show a) => String -> a -> a -> TestM ()
assertEqual description expected actual =
  unless (expected == actual)
    $ failTest
    $ unlines ["Assertion failed: " ++ description, "  expected: " ++ show expected, "  actual:   " ++ show actual]

-- | Checks both stdout and stderr.
assertOutputContains :: T.Text -> ProcessResult -> TestM ()
assertOutputContains expectedText result =
  unless (expectedText `T.isInfixOf` result.combinedOutput)
    $ failTest
    $ "Expected the command output to contain " ++ show expectedText ++ ". See the test log for the full output."

assertDirectoryExists :: FilePath -> TestM ()
assertDirectoryExists path = do
  exists <- resolvePath path >>= liftIO . doesDirectoryExist
  unless exists $ failTest $ "Expected directory to exist: " ++ path
