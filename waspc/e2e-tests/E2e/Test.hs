-- | Turns test steps into tasty tests.
module E2e.Test
  ( E2eTest (..),
    test,
    testGroup,
    sequentialTestGroup,
    toTestTree,
    runTestSteps,
  )
where

import Control.Concurrent.MVar (newMVar)
import Control.Exception (displayException, finally, fromException, throwIO)
import Control.Monad (forM_, (>=>))
import Data.IORef (newIORef, readIORef)
import Data.List (intercalate)
import E2e.Paths (getTestOutputsDir, getWaspcDir)
import E2e.TestM (RunConfig (..), TestEnv (..), TestFailure (..), TestLog (..), TestM, TestSkipped (..), runTestM)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Environment (lookupEnv)
import System.FilePath (joinPath, takeDirectory, (</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import qualified Test.Tasty as Tasty
import Test.Tasty.Providers (IsTest (..), Result, singleTest, testFailed, testPassed)
import UnliftIO.Exception (catch, tryAny)

-- | A tasty test tree that knows the names of the e2e groups it is in, which
-- decide where its tests run (see 'test').
newtype E2eTest = E2eTest ([String] -> Tasty.TestTree)

toTestTree :: E2eTest -> Tasty.TestTree
toTestTree (E2eTest makeTestTree) = makeTestTree []

-- | A test that passes when all of its steps succeed.
--
-- It runs in a fresh @test-outputs/<groups>/<name>/@ dir, and writes the
-- output of the commands it runs to @output.log@ in it.
test :: String -> TestM a -> E2eTest
test name steps = E2eTest $ \groupNames ->
  singleTest name . TestSteps $ do
    testDir <- (</> joinPath (groupNames ++ [name])) <$> getTestOutputsDir
    removePathForcibly testDir
    createDirectoryIfMissing True testDir
    (runTestSteps (intercalate "/" $ groupNames ++ [name]) testDir (testDir </> "output.log") steps >> return (testPassed ""))
      `catch` (\(TestFailure message) -> return $ testFailed message)
      `catch` (\(TestSkipped message) -> return $ testPassed message)

-- | Tests in the group run in parallel.
testGroup :: String -> [E2eTest] -> E2eTest
testGroup = makeGroup Tasty.testGroup

-- | Tests in the group run one after another, in order. For tests that can't
-- run at the same time, e.g. because they use the same port.
sequentialTestGroup :: String -> [E2eTest] -> E2eTest
sequentialTestGroup = makeGroup Tasty.inOrderTestGroup

makeGroup :: (String -> [Tasty.TestTree] -> Tasty.TestTree) -> String -> [E2eTest] -> E2eTest
makeGroup makeTastyGroup name tests = E2eTest $ \groupNames ->
  makeTastyGroup name [makeTestTree (groupNames ++ [name]) | E2eTest makeTestTree <- tests]

newtype TestSteps = TestSteps (IO Result)

instance IsTest TestSteps where
  run _ (TestSteps steps) _ = steps
  testOptions = pure []

-- | Runs the steps in the dir, with a new log file and cleanups.
--
-- Throws 'TestFailure' if a step fails (with a pointer to the log), and
-- 'TestSkipped' if the test skipped itself.
runTestSteps :: String -> FilePath -> FilePath -> TestM a -> IO a
runTestSteps testName workingDir logFile steps = do
  runConfig <- getRunConfig
  createDirectoryIfMissing True (takeDirectory logFile)
  withFile logFile WriteMode $ \logHandle -> do
    hPutStrLn logHandle $ "=== " ++ testName ++ " ==="
    logLock <- newMVar ()
    cleanupsRef <- newIORef []
    let testEnv =
          TestEnv
            { runConfig,
              workingDir,
              testLog = TestLog {logFile, logHandle, logLock},
              cleanups = cleanupsRef
            }
        runCleanups = do
          cleanups <- readIORef cleanupsRef
          forM_ cleanups $
            tryAny >=> either (hPutStrLn logHandle . ("Cleanup failed: " ++) . displayException) return

    tryAny (runTestM testEnv steps `finally` runCleanups) >>= \case
      Right result -> return result
      Left err
        | Just skipped@(TestSkipped _) <- fromException err -> throwIO skipped
        | Just (TestFailure message) <- fromException err -> throwIO . TestFailure $ withLogPointer message
        | otherwise -> throwIO . TestFailure . withLogPointer $ displayException err
  where
    withLogPointer message = message ++ "\nFull log: " ++ logFile

getRunConfig :: IO RunConfig
getRunConfig = do
  waspCliPath <- maybe (fail "WASP_CLI_CMD is not set.") return =<< lookupEnv "WASP_CLI_CMD"
  gitRootDir <- takeDirectory <$> getWaspcDir
  isDockerEnabled <- (`elem` [Nothing, Just ""]) <$> lookupEnv "WASP_E2E_TESTS_SKIP_DOCKER"
  return RunConfig {waspCliPath, gitRootDir, isDockerEnabled}
