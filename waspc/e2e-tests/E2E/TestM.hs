{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The monad test steps run in, and what every step can do: fail, label a
-- phase, run in a directory, register cleanups, log, and wait.
module E2E.TestM
  ( TestM,
    TestEnv (..),
    TestLog (..),
    TestFailure (..),
    runTestM,
    failTest,
    failureMessage,
    step,
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
    settleTimeoutSeconds,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, ResourceT, allocate, runResourceT)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Unique (hashUnique, newUnique)
import E2E.TestSuiteEnv (TestSuiteEnv)
import GHC.Stack (prettySrcLoc)
import System.FilePath ((</>))
import qualified System.FilePath as FP
import System.IO (Handle, hFlush, stderr)
import Test.HUnit.Lang (HUnitFailure (..), formatFailureReason)
import UnliftIO (MonadUnliftIO, throwIO)
import UnliftIO.Exception (Exception, SomeException, catchAny, displayException, fromException)

data TestEnv = TestEnv
  { testSuiteEnv :: TestSuiteEnv,
    -- | The test's groups and name, e.g. @wasp-start/fail-outside-project@.
    testName :: String,
    -- | The absolute directory relative paths are resolved against and
    -- commands run in.
    workingDir :: FilePath,
    testLog :: TestLog
  }

data TestLog = TestLog
  { logFile :: FilePath,
    logHandle :: Handle,
    -- | Output of concurrently running processes is written from several threads.
    logLock :: MVar ()
  }

-- | A sequence of test steps. Steps fail the test by throwing 'TestFailure'.
--
-- Its 'ResourceT' scope holds the actions to run when the steps end, no matter
-- how they end. The last registered one runs first.
newtype TestM a = TestM (ReaderT TestEnv (ResourceT IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadResource, MonadReader TestEnv)

newtype TestFailure = TestFailure String

-- | Shows only the message, since tasty and tasty-golden show an exception that
-- escapes a test (e.g. from a snapshot test's steps) with 'show'.
instance Show TestFailure where
  show (TestFailure message) = message

instance Exception TestFailure

-- | Runs the steps in a cleanup scope of their own, which runs the cleanups
-- they registered once they end.
runTestM :: TestEnv -> TestM a -> IO a
runTestM testEnv (TestM reader) = runResourceT $ runReaderT reader testEnv

failTest :: String -> TestM a
failTest = throwIO . TestFailure

-- | What a test that a step threw the exception in fails with.
--
-- hspec expectations and HUnit assertions (see "E2E.Assertions") throw
-- 'HUnitFailure', which only shows as a readable message when formatted. We
-- show where it failed, as GHC shows call stacks, then why.
failureMessage :: SomeException -> String
failureMessage err
  | Just (TestFailure message) <- fromException err = message
  | Just (HUnitFailure location reason) <- fromException err =
      maybe "" ((++ "\n") . prettySrcLoc) location ++ formatFailureReason reason
  | otherwise = displayException err

-- | Labels a phase of a long test: the label is logged as a header, and
-- prefixed to the message when a step in the phase fails, so that the failure
-- says which phase broke.
step :: String -> TestM a -> TestM a
step label steps = do
  logLine $ "\n### " ++ label
  -- Any exception, not only a 'TestFailure', so that e.g. an IO error in the
  -- step says which step it's in too. 'catchAny' lets asynchronous exceptions
  -- (e.g. a timeout or Ctrl+C) through untouched.
  steps `catchAny` \err -> failTest $ "In step " ++ show label ++ ": " ++ failureMessage err

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
-- already closed when they run. Give the commands a short timeout (see
-- 'E2E.Command.cleanupCommandTimeoutSeconds').
registerCleanupStep :: TestM () -> TestM ()
registerCleanupStep cleanupSteps = do
  testEnv <- ask
  void $ allocateWithCleanup (return ()) (const $ runTestM testEnv cleanupSteps)

-- | Writes the failure to the test's log, and to the test runner's stderr,
-- since the test still passes, and a passing test's output isn't shown with
-- @--hide-successes@.
reportCleanupFailure :: TestEnv -> SomeException -> IO ()
reportCleanupFailure testEnv err = do
  writeToLog testEnv.testLog (encode $ "Cleanup failed: " ++ failureMessage err ++ "\n")
  -- In one write, so that lines from tests running in parallel don't mix.
  BS.hPut stderr . encode $ "Cleanup failed in " ++ testEnv.testName ++ " (log: " ++ testEnv.testLog.logFile ++ "): " ++ failureMessage err ++ "\n"
  where
    encode = TE.encodeUtf8 . T.pack

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

-- | Checks the condition right away, and then every 'pollIntervalMicroseconds'
-- until it holds. Fails the test if it still doesn't hold after the given
-- number of seconds.
--
-- The deadline is only checked between checks, so a condition that runs a
-- command should give it a timeout no longer than the wait.
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
          else liftIO (threadDelay pollIntervalMicroseconds) >> poll
  poll

-- | Short enough not to slow tests down, long enough not to keep a core busy.
pollIntervalMicroseconds :: Int
pollIntervalMicroseconds = 500000

-- | For a wait on something that follows an event we already waited for, e.g.
-- a process's children stopping after it exited.
settleTimeoutSeconds :: Int
settleTimeoutSeconds = 30
