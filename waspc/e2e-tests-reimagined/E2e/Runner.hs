module E2e.Runner
  ( RunOptions (..),
    runTests,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel, wait, withAsync)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.STM
  ( TVar,
    atomically,
    check,
    modifyTVar',
    newTVarIO,
    readTVar,
    readTVarIO,
    retry,
    writeTVar,
  )
import Control.Exception (SomeException, displayException, finally, onException)
import Control.Monad (forM_, forever, unless, void, when)
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import E2e.Paths
  ( currentSnapshotDirInSnapshotsDir,
    getGoldenSnapshotsDir,
    getTestOutputsDir,
    goldenSnapshotDirInSnapshotsDir,
    snapshotLogFileInSnapshotsDir,
    snapshotsDirInTestOutputsDir,
    testDirInTestOutputsDir,
    testLogFileInTestDir,
  )
import E2e.Resource (Resource (..))
import E2e.Snapshot (SnapshotMode (..), SnapshotOutcome (..), finalizeSnapshot)
import E2e.Test (E2eTest (..), TestKind (..), fullTestName)
import E2e.TestM (RunConfig (..), TestEnv (..), TestLog (..), runTestM, writeToLog)
import StrongPath (Abs, Dir', File', Path', castDir, castFile, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, doesPathExist, removeDirectoryRecursive)
import System.IO (IOMode (WriteMode), hIsTerminalDevice, stdout, withFile)
import Text.Printf (printf)
import UnliftIO.Exception (tryAny)

data RunOptions = RunOptions
  { maxParallelJobs :: Int,
    snapshotMode :: SnapshotMode
  }

data TestOutcome
  = Passed
  | Failed String
  | Skipped String

data TestResult = TestResult
  { testName :: String,
    outcome :: TestOutcome,
    duration :: NominalDiffTime,
    logFile :: FilePath
  }

-- | Runs the tests in parallel and reports each result as soon as it's known.
-- Returns whether all tests passed.
--
-- Tests are started in the order they're given in, as long as there is a free
-- job slot and all of the test's resources are available. A test that waits
-- for a resource doesn't block the tests after it.
runTests :: RunConfig -> RunOptions -> [E2eTest] -> IO Bool
runTests runConfig runOptions tests = do
  reporter <- newReporter (length tests)
  printLine reporter $
    "Running " ++ show (length tests) ++ " e2e tests with up to " ++ show runOptions.maxParallelJobs ++ " in parallel."

  pendingTests <- newTVarIO tests
  freeJobSlots <- newTVarIO runOptions.maxParallelJobs
  resourceUsage <- newTVarIO Map.empty
  runningTests <- newTVarIO Map.empty
  resultsRef <- newIORef []
  testThreadsRef <- newIORef []

  let startNextTest :: IO Bool
      startNextTest = do
        maybeTest <- atomically $ do
          pending <- readTVar pendingTests
          if null pending
            then return Nothing
            else do
              readTVar freeJobSlots >>= check . (> 0)
              usage <- readTVar resourceUsage
              case break (areResourcesAvailable usage) pending of
                (_, []) -> retry
                (testsBefore, nextTest : testsAfter) -> do
                  writeTVar pendingTests (testsBefore ++ testsAfter)
                  modifyTVar' freeJobSlots (subtract 1)
                  modifyTVar' resourceUsage (acquireResources nextTest.resources)
                  return (Just nextTest)
        case maybeTest of
          Nothing -> return False
          Just nextTest -> do
            startTime <- getCurrentTime
            atomically $ modifyTVar' runningTests (Map.insert (fullTestName nextTest) startTime)
            testThread <- async $ do
              result <- runTest runConfig runOptions nextTest `finally` releaseTest nextTest
              modifyIORef' resultsRef (result :)
              reportResult reporter result
            modifyIORef' testThreadsRef (testThread :)
            return True

      releaseTest :: E2eTest -> IO ()
      releaseTest finishedTest = atomically $ do
        modifyTVar' freeJobSlots (+ 1)
        modifyTVar' resourceUsage (releaseResources finishedTest.resources)
        modifyTVar' runningTests (Map.delete (fullTestName finishedTest))

      startAllTests = do
        started <- startNextTest
        when started startAllTests

      waitForAllTests = readIORef testThreadsRef >>= mapM_ wait

      cancelAllTests = readIORef testThreadsRef >>= mapM_ cancel

  withAsync (reportRunningTestsPeriodically reporter runningTests) $ \_ ->
    (startAllTests >> waitForAllTests) `onException` cancelAllTests

  results <- reverse <$> readIORef resultsRef
  reportSummary reporter results
  return $ not (any isFailure results)

areResourcesAvailable :: Map.Map String Int -> E2eTest -> Bool
areResourcesAvailable usage e2eTest = all isAvailable e2eTest.resources
  where
    isAvailable resource = Map.findWithDefault 0 resource.name usage < resource.capacity

acquireResources :: [Resource] -> Map.Map String Int -> Map.Map String Int
acquireResources resources usage = foldr (\resource -> Map.insertWith (+) resource.name 1) usage resources

releaseResources :: [Resource] -> Map.Map String Int -> Map.Map String Int
releaseResources resources usage = foldr (\resource -> Map.adjust (subtract 1) resource.name) usage resources

isFailure :: TestResult -> Bool
isFailure result = case result.outcome of
  Failed _ -> True
  _ -> False

runTest :: RunConfig -> RunOptions -> E2eTest -> IO TestResult
runTest runConfig runOptions e2eTest = do
  startTime <- getCurrentTime
  testOutputsDir <- getTestOutputsDir
  let (workingDir, logFile) = case e2eTest.kind of
        PlainTest ->
          let testDir = testOutputsDir </> testDirInTestOutputsDir (e2eTest.groupPath ++ [e2eTest.name])
           in (castDir testDir, castFile $ testDir </> testLogFileInTestDir)
        SnapshotTest ->
          let snapshotsDir = testOutputsDir </> snapshotsDirInTestOutputsDir
           in ( castDir $ snapshotsDir </> currentSnapshotDirInSnapshotsDir e2eTest.name,
                castFile $ snapshotsDir </> snapshotLogFileInSnapshotsDir e2eTest.name
              )
  outcome <-
    if e2eTest.requiresDocker && not runConfig.isDockerEnabled
      then return $ Skipped "Docker is disabled via the WASP_E2E_TESTS_SKIP_DOCKER environment variable."
      else
        -- An error outside of the test steps (e.g. preparing its directory)
        -- fails only this test, instead of the whole run.
        either failedWith id <$> tryAny (recreateDirectory workingDir >> runTestSteps workingDir logFile)
  endTime <- getCurrentTime
  return
    TestResult
      { testName = fullTestName e2eTest,
        outcome,
        duration = diffUTCTime endTime startTime,
        logFile = SP.fromAbsFile logFile
      }
  where
    runTestSteps :: Path' Abs Dir' -> Path' Abs File' -> IO TestOutcome
    runTestSteps workingDir logFile =
      withFile (SP.fromAbsFile logFile) WriteMode $ \logHandle -> do
        logLock <- newMVar ()
        cleanupsRef <- newIORef []
        let testLog = TestLog {logFile = castFile logFile, logHandle, logLock}
            testEnv = TestEnv {runConfig, workingDir, testLog, cleanups = cleanupsRef}
        writeToLog testLog (BSC.pack $ "=== " ++ fullTestName e2eTest ++ " ===\n")
        -- 'tryAny' lets async exceptions through, so cancelled tests stop right away.
        stepsResult <- tryAny (runTestM testEnv e2eTest.steps) `finally` (readIORef cleanupsRef >>= runCleanups)
        outcome <- case stepsResult of
          Left exception -> return $ failedWith exception
          Right () -> case e2eTest.kind of
            PlainTest -> return Passed
            SnapshotTest -> either failedWith id <$> tryAny (checkSnapshot workingDir)
        case outcome of
          Failed message -> writeToLog testLog (BSC.pack $ "\n=== FAILED ===\n" ++ message ++ "\n")
          _ -> return ()
        return outcome

    checkSnapshot :: Path' Abs Dir' -> IO TestOutcome
    checkSnapshot currentSnapshotDir = do
      goldenSnapshotDir <- (</> goldenSnapshotDirInSnapshotsDir e2eTest.name) <$> getGoldenSnapshotsDir
      snapshotOutcome <- finalizeSnapshot runOptions.snapshotMode (castDir currentSnapshotDir) goldenSnapshotDir
      return $ case snapshotOutcome of
        SnapshotMatches -> Passed
        SnapshotAccepted -> Passed
        SnapshotDiffers differences -> Failed differences

failedWith :: SomeException -> TestOutcome
failedWith = Failed . displayException

-- | Runs each cleanup even if the ones before it fail.
runCleanups :: [IO ()] -> IO ()
runCleanups = mapM_ $ \cleanup -> void (tryAny cleanup)

recreateDirectory :: Path' Abs Dir' -> IO ()
recreateDirectory dir = do
  let dirPath = SP.fromAbsDir dir
  exists <- doesPathExist dirPath
  when exists $ removeDirectoryRecursive dirPath
  createDirectoryIfMissing True dirPath

data Reporter = Reporter
  { totalTests :: Int,
    finishedTests :: TVar Int,
    outputLock :: MVar (),
    useColors :: Bool,
    runStartTime :: UTCTime
  }

newReporter :: Int -> IO Reporter
newReporter totalTests = do
  finishedTests <- newTVarIO 0
  outputLock <- newMVar ()
  useColors <- hIsTerminalDevice stdout
  runStartTime <- getCurrentTime
  return Reporter {totalTests, finishedTests, outputLock, useColors, runStartTime}

printLine :: Reporter -> String -> IO ()
printLine reporter line = withMVar reporter.outputLock $ \() -> putStrLn line

-- | e.g. @[ 12/58  21%] PASS wasp-build/fail-sqlite-project (12.3s)@
reportResult :: Reporter -> TestResult -> IO ()
reportResult reporter result = do
  finishedCount <- atomically $ modifyTVar' reporter.finishedTests (+ 1) >> readTVar reporter.finishedTests
  let percentage = (finishedCount * 100) `div` max 1 reporter.totalTests
      totalWidth = length (show reporter.totalTests)
      progress = printf "[%*d/%d %3d%%]" totalWidth finishedCount reporter.totalTests percentage :: String
      headline = unwords [progress, outcomeLabel reporter result.outcome, result.testName, "(" ++ formatDuration result.duration ++ ")"]
  printLine reporter $ case result.outcome of
    Passed -> headline
    Skipped reason -> headline ++ "\n" ++ indent reason
    Failed message -> headline ++ "\n" ++ indent message ++ "\n" ++ indent ("Log: " ++ result.logFile)

-- | Shows which tests are still running, so that a long quiet period doesn't look like a hang.
reportRunningTestsPeriodically :: Reporter -> TVar (Map.Map String UTCTime) -> IO ()
reportRunningTestsPeriodically reporter runningTests = forever $ do
  threadDelay (60 * 1000000)
  running <- readTVarIO runningTests
  now <- getCurrentTime
  unless (Map.null running)
    $ printLine reporter
    $ "  ... still running ("
      ++ show (Map.size running)
      ++ "): "
      ++ intercalate ", " [name ++ " " ++ formatDuration (diffUTCTime now startTime) | (name, startTime) <- Map.toList running]

reportSummary :: Reporter -> [TestResult] -> IO ()
reportSummary reporter results = do
  endTime <- getCurrentTime
  let passedCount = length [() | TestResult {outcome = Passed} <- results]
      skippedCount = length [() | TestResult {outcome = Skipped _} <- results]
      failedResults = filter isFailure results
  printLine reporter $
    "\n"
      ++ show passedCount
      ++ " passed, "
      ++ show (length failedResults)
      ++ " failed, "
      ++ show skippedCount
      ++ " skipped in "
      ++ formatDuration (diffUTCTime endTime reporter.runStartTime)
      ++ "."
  unless (null failedResults) $ do
    printLine reporter "Failed tests:"
    forM_ failedResults $ \result -> printLine reporter $ "  " ++ result.testName ++ "  (log: " ++ result.logFile ++ ")"

outcomeLabel :: Reporter -> TestOutcome -> String
outcomeLabel reporter = \case
  Passed -> colored "32" "PASS"
  Failed _ -> colored "31" "FAIL"
  Skipped _ -> colored "33" "SKIP"
  where
    colored colorCode label
      | reporter.useColors = "\ESC[" ++ colorCode ++ "m" ++ label ++ "\ESC[0m"
      | otherwise = label

indent :: String -> String
indent = intercalate "\n" . map ("    " ++) . lines

formatDuration :: NominalDiffTime -> String
formatDuration duration
  | totalSeconds < 60 = printf "%.1fs" totalSeconds
  | otherwise = printf "%dm%02ds" (wholeSeconds `div` 60) (wholeSeconds `mod` 60)
  where
    totalSeconds = realToFrac duration :: Double
    wholeSeconds = floor totalSeconds :: Int
