module E2e.Assertions
  ( assertTrue,
    assertEqual,
    assertOutputContains,
    assertOutputDoesNotContain,
    assertValidJson,
    assertFileExists,
    assertDirectoryExists,
    assertDirectoryDoesNotExist,
    assertSymlinkExists,
  )
where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2e.Process (ProcessResult, combinedOutput)
import E2e.TestM (TestM, failTest, resolvePath)
import System.Directory (doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
import UnliftIO.Exception (catchAny)

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
  unless (expectedText `T.isInfixOf` combinedOutput result)
    $ failTest
    $ "Expected the command output to contain " ++ show expectedText ++ ". See the test log for the full output."

assertOutputDoesNotContain :: T.Text -> ProcessResult -> TestM ()
assertOutputDoesNotContain unexpectedText result =
  when (unexpectedText `T.isInfixOf` combinedOutput result)
    $ failTest
    $ "Expected the command output not to contain " ++ show unexpectedText ++ ". See the test log for the full output."

assertValidJson :: T.Text -> TestM Aeson.Value
assertValidJson text =
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 text) of
    Left err -> failTest $ "Expected valid JSON, but parsing failed: " ++ err
    Right value -> return value

assertFileExists :: FilePath -> TestM ()
assertFileExists = assertPath doesFileExist "Expected file to exist: "

assertDirectoryExists :: FilePath -> TestM ()
assertDirectoryExists = assertPath doesDirectoryExist "Expected directory to exist: "

assertDirectoryDoesNotExist :: FilePath -> TestM ()
assertDirectoryDoesNotExist = assertPath (fmap not . doesDirectoryExist) "Expected directory not to exist: "

assertSymlinkExists :: FilePath -> TestM ()
assertSymlinkExists = assertPath (\path -> pathIsSymbolicLink path `catchAny` const (return False)) "Expected symlink to exist: "

assertPath :: (FilePath -> IO Bool) -> String -> FilePath -> TestM ()
assertPath check message path = do
  absPath <- resolvePath path
  holds <- liftIO $ check absPath
  unless holds $ failTest $ message ++ path
