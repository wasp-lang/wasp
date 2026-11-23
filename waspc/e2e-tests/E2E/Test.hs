-- | Turns test steps into tasty tests.
module E2E.Test
  ( E2ETest (..),
    test,
    testGroup,
    sequentialTestGroup,
    toTestTree,
    checkOutputDirsAreUnique,
    runTestSteps,
  )
where

import Control.Concurrent.MVar (newMVar)
import Control.Exception (displayException, fromException, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (getInternalState, runResourceT)
import Data.List (intercalate, isPrefixOf, nub)
import E2E.Paths (getTestOutputsDir)
import E2E.RunConfig (RunConfig)
import E2E.TestM (TestEnv (..), TestFailure (..), TestLog (..), TestM, TestSkipped (..), runTestM)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (joinPath, splitDirectories, takeDirectory, (</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import qualified Test.Tasty as Tasty
import Test.Tasty.Providers (IsTest (..), Result, singleTest, testFailed, testPassed)
import UnliftIO.Exception (catch, tryAny)

-- | Tests that know the names of the e2e groups they are in, which decide
-- where they run (see 'test').
data E2ETest = E2ETest
  { -- | The dirs the tests run in, relative to the test outputs dir. They
    -- delete these dirs, so no two tests may share one.
    outputDirs :: [String] -> [FilePath],
    makeTestTree :: RunConfig -> [String] -> Tasty.TestTree
  }

toTestTree :: RunConfig -> E2ETest -> Tasty.TestTree
toTestTree runConfig e2eTest = e2eTest.makeTestTree runConfig []

-- | Fails if two tests would run in the same dir, or one in another's dir.
checkOutputDirsAreUnique :: [E2ETest] -> IO ()
checkOutputDirsAreUnique e2eTests =
  unless (null clashingDirs)
    $ fail
    $ "Some e2e tests would run in the same directory, so their names must change: " ++ intercalate ", " clashingDirs
  where
    dirs = map splitDirectories $ concatMap (`outputDirs` []) e2eTests
    clashingDirs =
      nub
        [ joinPath dir
        | (index, dir) <- zip [0 :: Int ..] dirs,
          (otherIndex, otherDir) <- zip [0 ..] dirs,
          index /= otherIndex,
          dir `isPrefixOf` otherDir
        ]

-- | A test that passes when all of its steps succeed.
--
-- It runs in a fresh @test-outputs/<groups>/<name>/@ dir, and writes the
-- output of the commands it runs to @output.log@ in it.
test :: String -> TestM a -> E2ETest
test name steps =
  E2ETest
    { outputDirs = \groupNames -> [testDirInTestOutputsDir groupNames],
      makeTestTree = \runConfig groupNames ->
        singleTest name . TestSteps $ do
          testDir <- (</> testDirInTestOutputsDir groupNames) <$> getTestOutputsDir
          removePathForcibly testDir
          createDirectoryIfMissing True testDir
          (runTestSteps runConfig (intercalate "/" $ groupNames ++ [name]) testDir (testDir </> "output.log") steps >> return (testPassed ""))
            `catch` (\(TestFailure message) -> return $ testFailed message)
            `catch` (\(TestSkipped message) -> return $ testPassed message)
    }
  where
    testDirInTestOutputsDir groupNames = joinPath (groupNames ++ [name])

-- | Tests in the group run in parallel.
testGroup :: String -> [E2ETest] -> E2ETest
testGroup = makeGroup Tasty.testGroup

-- | Tests in the group run one after another, in order. For tests that can't
-- run at the same time, e.g. because they use the same port.
sequentialTestGroup :: String -> [E2ETest] -> E2ETest
sequentialTestGroup = makeGroup Tasty.inOrderTestGroup

makeGroup :: (String -> [Tasty.TestTree] -> Tasty.TestTree) -> String -> [E2ETest] -> E2ETest
makeGroup makeTastyGroup name tests =
  E2ETest
    { outputDirs = \groupNames -> concatMap (`outputDirs` (groupNames ++ [name])) tests,
      makeTestTree = \runConfig groupNames ->
        makeTastyGroup name [e2eTest.makeTestTree runConfig (groupNames ++ [name]) | e2eTest <- tests]
    }

newtype TestSteps = TestSteps (IO Result)

instance IsTest TestSteps where
  run _ (TestSteps steps) _ = steps
  testOptions = pure []

-- | Runs the steps in the dir, with a new log file and cleanups.
--
-- Throws 'TestFailure' if a step fails (with a pointer to the log), and
-- 'TestSkipped' if the test skipped itself.
runTestSteps :: RunConfig -> String -> FilePath -> FilePath -> TestM a -> IO a
runTestSteps runConfig testName workingDir logFile steps = do
  createDirectoryIfMissing True (takeDirectory logFile)
  withFile logFile WriteMode $ \logHandle -> do
    hPutStrLn logHandle $ "=== " ++ testName ++ " ==="
    logLock <- newMVar ()
    let runStepsAndCleanups = runResourceT $ do
          resources <- getInternalState
          liftIO $
            runTestM
              TestEnv
                { runConfig,
                  workingDir,
                  testLog = TestLog {logFile, logHandle, logLock},
                  resources
                }
              steps

    tryAny runStepsAndCleanups >>= \case
      Right result -> return result
      Left err
        | Just skipped@(TestSkipped _) <- fromException err -> throwIO skipped
        | Just (TestFailure message) <- fromException err -> throwIO . TestFailure $ withLogPointer message
        | otherwise -> throwIO . TestFailure . withLogPointer $ displayException err
  where
    withLogPointer message = message ++ "\nFull log: " ++ logFile
