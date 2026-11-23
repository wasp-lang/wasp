-- | Steps that fail the test when a check doesn't hold.
module E2E.Assertions
  ( assertTrue,
    assertEqual,
    assertOutputContains,
    assertProcessOutputContains,
    assertDirectoryExists,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import E2E.Process (ProcessResult (..), RunningProcess, failWithOutput, failWithResult, getOutput)
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
  unless (expectedText `T.isInfixOf` result.combinedOutput) $
    failWithResult result ("didn't print " ++ show expectedText)

-- | Like 'assertOutputContains', for what a process printed so far.
assertProcessOutputContains :: T.Text -> RunningProcess -> TestM ()
assertProcessOutputContains expectedText runningProcess = do
  output <- getOutput runningProcess
  unless (expectedText `T.isInfixOf` output) $
    failWithOutput runningProcess ("didn't print " ++ show expectedText)

assertDirectoryExists :: FilePath -> TestM ()
assertDirectoryExists path = do
  exists <- resolvePath path >>= liftIO . doesDirectoryExist
  unless exists $ failTest $ "Expected directory to exist: " ++ path
