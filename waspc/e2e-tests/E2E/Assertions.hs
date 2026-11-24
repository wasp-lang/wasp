-- | Steps that fail the test when a check doesn't hold.
module E2E.Assertions
  ( assertTrue,
    assertEqual,
    assertOutputContains,
    assertStdoutContains,
    assertDirectoryExists,
    assertDirectoryDoesNotExist,
    assertFileExists,
    assertSymlinkExists,
    assertStdoutJsonField,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E.Process (ProcessResult (..), failWithResult)
import E2E.TestM (TestM, failTest, resolvePath)
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
  unless (expectedText `T.isInfixOf` result.combinedOutput) $
    failWithResult result ("didn't print " ++ show expectedText)

-- | Like 'assertOutputContains', for when it matters that the text is on
-- stdout.
assertStdoutContains :: T.Text -> ProcessResult -> TestM ()
assertStdoutContains expectedText result =
  unless (expectedText `T.isInfixOf` result.stdout) $
    failWithResult result ("didn't print " ++ show expectedText ++ " to stdout")

-- | Checks that stdout is a single JSON object, with the field set to the
-- value.
assertStdoutJsonField :: Aeson.Key -> Aeson.Value -> ProcessResult -> TestM ()
assertStdoutJsonField key expectedValue result =
  case Aeson.eitherDecodeStrict (TE.encodeUtf8 result.stdout) of
    Left err -> failWithResult result ("printed invalid JSON to stdout: " ++ err)
    Right (Aeson.Object object)
      | KeyMap.lookup key object == Just expectedValue -> return ()
      | otherwise ->
          failWithResult result $
            "printed JSON whose " ++ show key ++ " is " ++ maybe "missing" show (KeyMap.lookup key object) ++ ", instead of " ++ show expectedValue
    Right _ -> failWithResult result "printed JSON to stdout that isn't an object"

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
