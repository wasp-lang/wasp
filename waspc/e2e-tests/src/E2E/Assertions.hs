-- | Steps that fail the test when a check doesn't hold.
--
-- The general ones are hspec's expectations, in their
-- 'Control.Monad.IO.Class.MonadIO' version. Their failures say where they
-- failed (see 'E2E.TestM.failureMessage'). The rest are for checks that hspec's
-- expectations don't fail with a useful message.
module E2E.Assertions
  ( module Test.Hspec.Expectations.Lifted,
    assertOutputContains,
    assertStdoutContains,
    assertExitsSuccessfully,
    assertExitsWithFailure,
    assertDirectoryExists,
    assertDirectoryDoesNotExist,
    assertFileExists,
    assertSymlinkExists,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import E2E.Process (ExpectedExit (..), ProcessResult (..), RunningProcess, failOnUnexpectedExit, failWithResult, readResult, waitForExit)
import E2E.TestM (TestM, failTest, resolvePath)
import System.Directory (doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
import Test.Hspec.Expectations.Lifted hiding (HasCallStack)
import UnliftIO.Exception (catchAny)

-- | Checks both stdout and stderr.
assertOutputContains :: T.Text -> ProcessResult -> TestM ()
assertOutputContains expectedText result =
  unless (expectedText `T.isInfixOf` result.combinedOutput) $
    failWithResult result ("didn't print " ++ show expectedText)

-- | Like 'assertOutputContains', for when it matters that the text is on
-- stdout.
assertStdoutContains :: T.Text -> ProcessResult -> TestM ()
assertStdoutContains expectedText result =
  unless (expectedText `T.isInfixOf` result.stdout) $
    failWithResult result ("didn't print " ++ show expectedText ++ " to stdout")

-- | Waits for the process to exit, but at most for the given number of
-- seconds, and fails the test with its output if it doesn't exit in time or
-- doesn't succeed.
assertExitsSuccessfully :: RunningProcess -> Int -> TestM ProcessResult
assertExitsSuccessfully = assertExits ExpectSuccess

-- | Like 'assertExitsSuccessfully', but the process must exit with an error,
-- and not be stopped by a signal (e.g. when it crashes).
assertExitsWithFailure :: RunningProcess -> Int -> TestM ProcessResult
assertExitsWithFailure = assertExits ExpectFailure

assertExits :: ExpectedExit -> RunningProcess -> Int -> TestM ProcessResult
assertExits expectedExit runningProcess seconds = do
  result <- waitForExit runningProcess seconds >>= readResult runningProcess
  failOnUnexpectedExit expectedExit result
  return result

assertDirectoryExists :: FilePath -> TestM ()
assertDirectoryExists = assertPath doesDirectoryExist "Expected directory to exist: "

assertDirectoryDoesNotExist :: FilePath -> TestM ()
assertDirectoryDoesNotExist = assertPath (fmap not . doesDirectoryExist) "Expected directory not to exist: "

assertFileExists :: FilePath -> TestM ()
assertFileExists = assertPath doesFileExist "Expected file to exist: "

assertSymlinkExists :: FilePath -> TestM ()
assertSymlinkExists =
  -- `pathIsSymbolicLink` throws when the path doesn't exist, which is just
  -- a failed assertion here.
  assertPath (\path -> pathIsSymbolicLink path `catchAny` const (return False)) "Expected symlink to exist: "

assertPath :: (FilePath -> IO Bool) -> String -> FilePath -> TestM ()
assertPath check message path = do
  absPath <- resolvePath path
  holds <- liftIO $ check absPath
  unless holds $ failTest $ message ++ absPath
