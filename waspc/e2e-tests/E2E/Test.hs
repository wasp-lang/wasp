-- | Turns test steps into tasty tests.
module E2E.Test
  ( E2ETest (..),
    test,
    testGroup,
    sequentialTestGroup,
    toTestTree,
    checkOutputDirs,
    TestPaths (..),
    runTestSteps,
    TestSteps (..),
  )
where

import Control.Concurrent.MVar (newMVar)
import Control.Exception (displayException, fromException, throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (getInternalState, runResourceT)
import Data.List (intercalate, isPrefixOf, nub)
import E2E.RunConfig (RunConfig (..))
import E2E.TestM (TestEnv (..), TestFailure (..), TestLog (..), TestM, runTestM)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (isPathSeparator, joinPath, takeDirectory, (</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import qualified Test.Tasty as Tasty
import Test.Tasty.Providers (IsTest (..), Result, singleTest, testFailed, testPassed)
import UnliftIO.Exception (catch, tryAny)

-- | Tests that know the names of the e2e groups they are in, which decide
-- where they run (see 'test').
data E2ETest = E2ETest
  { -- | The dirs the tests run in, relative to the test outputs dir, as the
    -- names of the dirs on the way, e.g. @["wasp-start", "stop-on-sigint"]@.
    -- Tests delete these dirs, so no two tests may share one.
    outputDirs :: [String] -> [[String]],
    makeTestTree :: RunConfig -> [String] -> Tasty.TestTree
  }

toTestTree :: RunConfig -> E2ETest -> Tasty.TestTree
toTestTree runConfig e2eTest = e2eTest.makeTestTree runConfig []

-- | Fails if a test or group name isn't a single dir name (e.g. it's empty or
-- has a slash), so that a test could run in another's dir or outside of the
-- test outputs dir, or if two tests would run in the same dir, or one in
-- another's dir.
checkOutputDirs :: [E2ETest] -> IO ()
checkOutputDirs e2eTests = do
  unless (null invalidNames)
    $ fail
    $ "Some e2e test or group names aren't a single directory name, e.g. they're empty or have a slash: " ++ intercalate ", " (map show invalidNames)
  unless (null clashingDirs)
    $ fail
    $ "Some e2e tests would run in the same directory, so their names must change: " ++ intercalate ", " clashingDirs
  where
    dirs = concatMap (`outputDirs` []) e2eTests
    invalidNames = nub [name | dir <- dirs, name <- dir, not (isSingleDirName name)]
    isSingleDirName name = name `notElem` ["", ".", ".."] && not (any isPathSeparator name)
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
    { outputDirs = \groupNames -> [groupNames ++ [name]],
      makeTestTree = \runConfig groupNames ->
        singleTest name . TestSteps $ do
          let testPath = groupNames ++ [name]
              testDir = runConfig.testOutputsDir </> joinPath testPath
              testPaths = TestPaths {workingDir = testDir, logFile = testDir </> testLogFileName}
          (runTestSteps runConfig (intercalate "/" testPath) testPaths steps >> return (testPassed ""))
            `catch` (\(TestFailure message) -> return $ testFailed message)
    }

testLogFileName :: FilePath
testLogFileName = "output.log"

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

-- | A tasty test that runs the IO action to get its result.
newtype TestSteps = TestSteps (IO Result)

instance IsTest TestSteps where
  run _ (TestSteps steps) _ = steps
  testOptions = pure []

-- | Where a test's steps run, and where they log to. A record, so that the
-- two paths can't be swapped: the working dir is deleted.
data TestPaths = TestPaths
  { -- | Deleted and created again, empty, before the steps run.
    workingDir :: FilePath,
    logFile :: FilePath
  }

-- | Runs the steps in a fresh working dir, with a new log file and cleanups.
--
-- Throws 'TestFailure' if a step fails (with a pointer to the log).
runTestSteps :: RunConfig -> String -> TestPaths -> TestM a -> IO a
runTestSteps runConfig testName TestPaths {workingDir, logFile} steps = do
  removePathForcibly workingDir
  createDirectoryIfMissing True workingDir
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
                  testName,
                  workingDir,
                  testLog = TestLog {logFile, logHandle, logLock},
                  resources
                }
              steps

    tryAny runStepsAndCleanups >>= \case
      Right result -> return result
      Left err
        | Just (TestFailure message) <- fromException err -> throwIO . TestFailure $ withLogPointer message
        | otherwise -> throwIO . TestFailure . withLogPointer $ displayException err
  where
    withLogPointer message = message ++ "\nFull log: " ++ logFile
