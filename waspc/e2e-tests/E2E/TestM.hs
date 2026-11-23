{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The monad test steps run in, and what every step can do: fail, skip, run
-- in a directory, register cleanups, log, and wait.
module E2E.TestM
  ( TestM,
    TestEnv (..),
    TestLog (..),
    TestFailure (..),
    TestSkipped (..),
    runTestM,
    failTest,
    skipIfDockerDisabled,
    whenDockerEnabled,
    inDirectory,
    getWorkingDir,
    resolvePath,
    allocateWithCleanup,
    registerCleanupStep,
    makeUniqueName,
    writeToLog,
    logLine,
    waitUntil,
    pollUntil,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.Trans.Resource (InternalState, MonadResource (..), ReleaseKey, allocate, getInternalState, runInternalState, runResourceT)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Unique (hashUnique, newUnique)
import E2E.RunConfig (RunConfig (..), skipDockerEnvVar)
import System.FilePath ((</>))
import qualified System.FilePath as FP
import System.IO (Handle, hFlush, hPutStr, stderr)
import UnliftIO (MonadUnliftIO, throwIO)
import UnliftIO.Exception (Exception, SomeException, catchAny, displayException)

data TestEnv = TestEnv
  { runConfig :: RunConfig,
    -- | The test's groups and name, e.g. @wasp-start/fail-outside-project@.
    testName :: String,
    -- | The absolute directory relative paths are resolved against and
    -- commands run in.
    workingDir :: FilePath,
    testLog :: TestLog,
    -- | Holds the actions to run when the test ends, no matter how it ends.
    -- The last registered one runs first.
    resources :: InternalState
  }

data TestLog = TestLog
  { logFile :: FilePath,
    logHandle :: Handle,
    -- | Output of concurrently running processes is written from several threads.
    logLock :: MVar ()
  }

-- | A sequence of test steps. Steps fail the test by throwing 'TestFailure'.
newtype TestM a = TestM (ReaderT TestEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader TestEnv)

instance MonadResource TestM where
  liftResourceT action = asks (.resources) >>= liftIO . runInternalState action

newtype TestFailure = TestFailure String

instance Show TestFailure where
  show (TestFailure message) = message

instance Exception TestFailure

-- | Ends the test early, as passed.
newtype TestSkipped = TestSkipped String
  deriving (Show)

instance Exception TestSkipped

runTestM :: TestEnv -> TestM a -> IO a
runTestM testEnv (TestM reader) = runReaderT reader testEnv

failTest :: String -> TestM a
failTest = throwIO . TestFailure

-- | Ends the test early, as passed, when Docker is disabled. Steps before it
-- still run.
--
-- Not for snapshot tests, which must produce a snapshot: use
-- 'whenDockerEnabled' in them.
skipIfDockerDisabled :: TestM ()
skipIfDockerDisabled = do
  isDockerEnabled <- asks (.runConfig.isDockerEnabled)
  unless isDockerEnabled $ throwIO $ TestSkipped dockerDisabledMessage

-- | For steps that only some part of a test needs Docker for.
whenDockerEnabled :: TestM () -> TestM ()
whenDockerEnabled steps = do
  isDockerEnabled <- asks (.runConfig.isDockerEnabled)
  if isDockerEnabled then steps else logLine dockerDisabledMessage

dockerDisabledMessage :: String
dockerDisabledMessage = "SKIPPED: Docker is disabled via the " ++ skipDockerEnvVar ++ " environment variable."

-- | Runs the steps with the working directory set to a path relative to the
-- current one.
inDirectory :: FilePath -> TestM a -> TestM a
inDirectory dir steps = do
  absDir <- resolvePath dir
  local (\testEnv -> testEnv {workingDir = absDir}) steps

getWorkingDir :: TestM FilePath
getWorkingDir = asks (.workingDir)

-- | Resolves a path relative to the working directory. Absolute paths stay as they are.
resolvePath :: FilePath -> TestM FilePath
resolvePath path = FP.normalise . (</> path) <$> getWorkingDir

-- | Acquires a resource and registers its cleanup, which runs when the test
-- ends, or earlier with 'Control.Monad.Trans.Resource.release'. The test can't
-- be cancelled in between. A cleanup that fails is reported (see
-- 'reportCleanupFailure'), and the others still run.
allocateWithCleanup :: IO a -> (a -> IO ()) -> TestM (ReleaseKey, a)
allocateWithCleanup acquire cleanup = do
  testEnv <- ask
  allocate acquire $ \resource -> cleanup resource `catchAny` reportCleanupFailure testEnv

-- | Registers steps that run when the test ends, no matter how it ends, e.g.
-- to remove a Docker container the test created. Registered steps run in
-- reverse order, and in the working directory they were registered in.
--
-- The steps run after the test's last step, so they can't fail the test, and
-- they shouldn't assert anything: a failure is only reported (see
-- 'reportCleanupFailure'). Use them to remove resources, and make them
-- tolerate resources that are already gone (e.g. with
-- 'E2E.Process.runAllowingFailure').
--
-- The steps can run commands: they get a cleanup scope of their own, which
-- stops the processes they started when they finish. The test's scope is
-- already closed when they run.
registerCleanupStep :: TestM () -> TestM ()
registerCleanupStep step = do
  testEnv <- ask
  let runStepInOwnScope = runResourceT $ do
        stepResources <- getInternalState
        liftIO $ runTestM testEnv {resources = stepResources} step
  void $ allocateWithCleanup (return ()) (const runStepInOwnScope)

-- | Writes the failure to the test's log, and to the test runner's stderr,
-- since the test still passes, and a passing test's output isn't shown with
-- @--hide-successes@.
reportCleanupFailure :: TestEnv -> SomeException -> IO ()
reportCleanupFailure testEnv err = do
  writeToLog testEnv.testLog (TE.encodeUtf8 . T.pack $ "Cleanup failed: " ++ displayException err ++ "\n")
  hPutStr stderr $ "Cleanup failed in " ++ testEnv.testName ++ " (log: " ++ testEnv.testLog.logFile ++ "): " ++ displayException err ++ "\n"

-- | The name with a suffix that no other test run uses, for resources outside
-- the test dir that runs from other checkouts could also create, e.g. Docker
-- image tags. Register a cleanup for the resource, since it's never reused.
makeUniqueName :: String -> TestM String
makeUniqueName name = liftIO $ do
  microseconds <- floor . (* 1000000) <$> getPOSIXTime
  unique <- newUnique
  return $ name ++ "-" ++ show (microseconds :: Integer) ++ "-" ++ show (hashUnique unique)

writeToLog :: TestLog -> BS.ByteString -> IO ()
writeToLog testLog content =
  withMVar testLog.logLock $ \() -> BS.hPut testLog.logHandle content >> hFlush testLog.logHandle

logLine :: String -> TestM ()
logLine line = do
  testLog <- asks (.testLog)
  liftIO $ writeToLog testLog (TE.encodeUtf8 . T.pack $ line ++ "\n")

-- | Checks the condition right away, and then every half a second until it
-- holds. Fails the test if it still doesn't hold after the given number of
-- seconds.
waitUntil :: String -> Int -> TestM Bool -> TestM ()
waitUntil description seconds condition = do
  holds <- pollUntil seconds condition
  unless holds $ failTest $ "Timed out after " ++ show seconds ++ "s waiting until " ++ description ++ "."

-- | Like 'waitUntil', but returns whether the condition held in time instead
-- of failing, for waits that fail with more context.
pollUntil :: Int -> TestM Bool -> TestM Bool
pollUntil seconds condition = do
  deadline <- liftIO $ addUTCTime (fromIntegral seconds) <$> getCurrentTime
  let poll = do
        holds <- condition
        now <- liftIO getCurrentTime
        if holds || now > deadline
          then return holds
          else liftIO (threadDelay 500000) >> poll
  poll
