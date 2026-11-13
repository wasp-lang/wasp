{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module E2E.TestM
  ( TestM,
    TestEnv (..),
    RunConfig (..),
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
    registerCleanup,
    registerCleanupStep,
    writeToLog,
    logLine,
    waitUntil,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local, runReaderT)
import Control.Monad.Trans.Resource (InternalState, MonadResource (..), ReleaseKey, allocate, runInternalState)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import System.FilePath ((</>))
import qualified System.FilePath as FP
import System.IO (Handle, hFlush)
import UnliftIO (MonadUnliftIO, throwIO)
import UnliftIO.Exception (Exception, catchAny, displayException)

-- | Settings shared by all tests in a run.
data RunConfig = RunConfig
  { -- | The Wasp CLI under test.
    waspCliPath :: FilePath,
    gitRootDir :: FilePath,
    -- | Docker can be disabled with the @WASP_E2E_TESTS_SKIP_DOCKER@ env var.
    isDockerEnabled :: Bool
  }

data TestEnv = TestEnv
  { runConfig :: RunConfig,
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
dockerDisabledMessage = "SKIPPED: Docker is disabled via the WASP_E2E_TESTS_SKIP_DOCKER environment variable."

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
-- be cancelled in between. A cleanup that fails is logged, and the others
-- still run.
allocateWithCleanup :: IO a -> (a -> IO ()) -> TestM (ReleaseKey, a)
allocateWithCleanup acquire cleanup = do
  testLog <- asks (.testLog)
  let logFailure err = writeToLog testLog (TE.encodeUtf8 . T.pack $ "Cleanup failed: " ++ displayException err ++ "\n")
  allocate acquire $ \resource -> cleanup resource `catchAny` logFailure

registerCleanup :: IO () -> TestM ()
registerCleanup cleanup = void $ allocateWithCleanup (return ()) (const cleanup)

-- | Like 'registerCleanup', but for a test step.
registerCleanupStep :: TestM () -> TestM ()
registerCleanupStep step = do
  testEnv <- ask
  registerCleanup $ runTestM testEnv step

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
  deadline <- liftIO $ addUTCTime (fromIntegral seconds) <$> getCurrentTime
  let poll = do
        holds <- condition
        unless holds $ do
          now <- liftIO getCurrentTime
          when (now > deadline) $ failTest $ "Timed out after " ++ show seconds ++ "s waiting until " ++ description ++ "."
          liftIO $ threadDelay 500000
          poll
  poll
