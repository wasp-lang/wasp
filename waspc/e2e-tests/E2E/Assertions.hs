-- | Steps that fail the test when a check doesn't hold.
--
-- The general ones are hspec's expectations, the ones the unit tests use. Any
-- other hspec expectation or HUnit assertion works in a step too, with
-- 'liftIO'. Their failures say where they failed (see
-- 'E2E.TestM.failureMessage').
module E2E.Assertions
  ( shouldBe,
    expectationFailure,
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
import GHC.Stack (HasCallStack)
import System.Directory (doesDirectoryExist, doesFileExist, pathIsSymbolicLink)
import qualified Test.Hspec.Expectations as Hspec
import UnliftIO.Exception (catchAny)

infix 1 `shouldBe`

-- | hspec's 'Hspec.shouldBe', in 'TestM'.
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> TestM ()
actual `shouldBe` expected = liftIO $ actual `Hspec.shouldBe` expected

-- | hspec's 'Hspec.expectationFailure', in 'TestM'. For checks that need to
-- say more than a comparison would.
expectationFailure :: (HasCallStack) => String -> TestM ()
expectationFailure = liftIO . Hspec.expectationFailure

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
