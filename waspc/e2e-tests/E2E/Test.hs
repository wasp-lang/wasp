-- | Turns test steps into tasty tests.
--
-- The tests are plain tasty 'TestTree's, so that tasty-discover can find them
-- (see @TastyDiscoverDriver.hs@). What they need from the run, the
-- 'RunConfig' and the names of the e2e groups they are in, they get from
-- tasty's options (see 'setRunConfig' and 'testGroup').
module E2E.Test
  ( TestTree,
    test,
    skippedTest,
    testGroup,
    sequentialTestGroup,
    setRunConfig,
    withRunConfig,
    claimOutputDir,
    checkOutputDirs,
    TestPaths (..),
    runTestSteps,
    TestSteps (..),
  )
where

import Control.Concurrent.MVar (newMVar)
import Control.Exception (throwIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (getInternalState, runResourceT)
import Data.List (intercalate, isPrefixOf, nub)
import Data.Tagged (Tagged (..))
import E2E.RunConfig (RunConfig (..))
import E2E.TestM (TestEnv (..), TestFailure (..), TestLog (..), TestM, failureMessage, runTestM)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (isPathSeparator, joinPath, takeDirectory, (</>))
import System.IO (IOMode (WriteMode), hPutStrLn, withFile)
import Test.Tasty (TestTree, adjustOption, askOption, localOption)
import qualified Test.Tasty as Tasty
import Test.Tasty.Options (IsOption (..), lookupOption)
import Test.Tasty.Providers (IsTest (..), Result, singleTest, testFailed, testPassed)
import Test.Tasty.Runners (TreeFold (..), foldTestTree, trivialFold)
import UnliftIO.Exception (catch, tryAny)

-- | Set for the whole test tree by the e2e tests' main.
newtype RunConfigOption = RunConfigOption RunConfig

instance IsOption RunConfigOption where
  defaultValue = error "The e2e tests need a run config. Run them with the e2e tests' main, which sets it."
  parseValue = const Nothing
  optionName = Tagged "e2e-run-config"
  optionHelp = Tagged "The settings of the e2e test run. Not a command line option."

setRunConfig :: RunConfig -> TestTree -> TestTree
setRunConfig = localOption . RunConfigOption

withRunConfig :: (RunConfig -> TestTree) -> TestTree
withRunConfig makeTestTree = askOption $ \(RunConfigOption runConfig) -> makeTestTree runConfig

-- | The names of the e2e groups a test is in, outermost first, which decide
-- where it runs (see 'test').
newtype GroupNames = GroupNames [String]

instance IsOption GroupNames where
  defaultValue = GroupNames []
  parseValue = const Nothing
  optionName = Tagged "e2e-group-names"
  optionHelp = Tagged "The e2e groups a test is in. Not a command line option."

-- | The dir a test runs in, relative to the test outputs dir, as the names of
-- the dirs on the way, e.g. @["wasp-start", "stop-on-sigint"]@. Tests delete
-- their dirs, so no two tests may share one (see 'checkOutputDirs').
newtype OutputDir = OutputDir (Maybe [String])

instance IsOption OutputDir where
  defaultValue = OutputDir Nothing
  parseValue = const Nothing
  optionName = Tagged "e2e-output-dir"
  optionHelp = Tagged "The dir an e2e test runs in. Not a command line option."

-- | Declares the dir the test runs in, so that the run can check that no
-- other test uses it.
claimOutputDir :: [String] -> TestTree -> TestTree
claimOutputDir = localOption . OutputDir . Just

-- | Fails if a test or group name isn't a single dir name (e.g. it's empty or
-- has a slash), so that a test could run in another's dir or outside of the
-- test outputs dir, or if two tests would run in the same dir, or one in
-- another's dir.
--
-- Checks all of the tests, also the ones a pattern leaves out.
checkOutputDirs :: TestTree -> IO ()
checkOutputDirs testTree = do
  unless (null invalidNames)
    $ fail
    $ "Some e2e test or group names aren't a single directory name, e.g. they're empty or have a slash: " ++ intercalate ", " (map show invalidNames)
  unless (null clashingDirs)
    $ fail
    $ "Some e2e tests would run in the same directory, so their names must change: " ++ intercalate ", " clashingDirs
  where
    dirs = foldTestTree trivialFold {foldSingle = \options _ _ -> [dir | OutputDir (Just dir) <- [lookupOption options]]} mempty testTree
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
test :: String -> TestM a -> TestTree
test name steps =
  withRunConfig $ \runConfig -> inTestDir name $ \testPath -> TestSteps $ do
    let testDir = runConfig.testOutputsDir </> joinPath testPath
        testPaths = TestPaths {workingDir = testDir, logFile = testDir </> testLogFileName}
    (runTestSteps runConfig (intercalate "/" testPath) testPaths steps >> return (testPassed ""))
      `catch` (\(TestFailure message) -> return $ testFailed message)

-- | A test that passes with the message, without running. It still claims the
-- dir it would run in, so that the dir check doesn't depend on which tests a
-- run skips.
skippedTest :: String -> String -> TestTree
skippedTest name message = inTestDir name . const . TestSteps . return $ testPassed message

-- | A single test that runs in @test-outputs/<groups>/<name>/@. Gets the
-- names of the dirs on the way.
inTestDir :: String -> ([String] -> TestSteps) -> TestTree
inTestDir name makeTestSteps =
  askOption $ \(GroupNames groupNames) ->
    let testPath = groupNames ++ [name]
     in claimOutputDir testPath . singleTest name $ makeTestSteps testPath

testLogFileName :: FilePath
testLogFileName = "output.log"

-- | Tests in the group run in parallel.
testGroup :: String -> [TestTree] -> TestTree
testGroup name = inGroup name . Tasty.testGroup name

-- | Tests in the group run one after another, in order. For tests that can't
-- run at the same time, e.g. because they use the same port.
sequentialTestGroup :: String -> [TestTree] -> TestTree
sequentialTestGroup name = inGroup name . Tasty.inOrderTestGroup name

inGroup :: String -> TestTree -> TestTree
inGroup name = adjustOption $ \(GroupNames groupNames) -> GroupNames (groupNames ++ [name])

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
      Left err -> throwIO . TestFailure . withLogPointer $ failureMessage err
  where
    withLogPointer message = message ++ "\nFull log: " ++ logFile
