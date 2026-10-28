{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module E2e.TestM
  ( TestM,
    TestEnv (..),
    RunConfig (..),
    TestLog (..),
    TestFailure (..),
    runTestM,
    failTest,
    inDirectory,
    resolvePath,
    getWorkingDir,
    registerCleanup,
    registerCleanupStep,
    writeToLog,
    logLine,
    whenDockerEnabled,
  )
where

import Control.Concurrent.MVar (MVar, withMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, local, runReaderT)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef')
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2e.Paths (GitRootDir, TestLogFile)
import StrongPath (Abs, Dir, Dir', File, Path')
import qualified StrongPath as SP
import System.FilePath ((</>))
import qualified System.FilePath as FP
import System.IO (Handle, hFlush)
import UnliftIO (MonadUnliftIO, throwIO)
import UnliftIO.Exception (Exception)

-- | Settings shared by all tests in a run.
data RunConfig = RunConfig
  { waspCliPath :: FilePath,
    gitRootDir :: Path' Abs (Dir GitRootDir),
    -- | Docker can be disabled with the @WASP_E2E_TESTS_SKIP_DOCKER@ env var.
    isDockerEnabled :: Bool
  }

data TestEnv = TestEnv
  { runConfig :: RunConfig,
    -- | The directory relative paths are resolved against and commands run in.
    workingDir :: Path' Abs Dir',
    testLog :: TestLog,
    -- | Actions to run when the test ends, no matter how it ends.
    cleanups :: IORef [IO ()]
  }

data TestLog = TestLog
  { logFile :: Path' Abs (File TestLogFile),
    logHandle :: Handle,
    -- | Output of concurrently running processes is written from several threads.
    logLock :: MVar ()
  }

-- | A sequence of test steps. Steps fail the test by throwing 'TestFailure'.
newtype TestM a = TestM (ReaderT TestEnv IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader TestEnv)

newtype TestFailure = TestFailure String

instance Show TestFailure where
  show (TestFailure message) = message

instance Exception TestFailure

runTestM :: TestEnv -> TestM a -> IO a
runTestM testEnv (TestM reader) = runReaderT reader testEnv

failTest :: String -> TestM a
failTest = throwIO . TestFailure

-- | Runs the steps with the working directory set to a path relative to the current one.
inDirectory :: FilePath -> TestM a -> TestM a
inDirectory relDir steps = do
  absDir <- resolvePath relDir >>= liftIO . SP.parseAbsDir
  local (\testEnv -> testEnv {workingDir = absDir}) steps

getWorkingDir :: TestM FilePath
getWorkingDir = asks (SP.fromAbsDir . (.workingDir))

-- | Resolves a path relative to the working directory.
resolvePath :: FilePath -> TestM FilePath
resolvePath path = FP.normalise . (</> path) <$> getWorkingDir

registerCleanup :: IO () -> TestM ()
registerCleanup cleanup = do
  cleanupsRef <- asks (.cleanups)
  liftIO $ modifyIORef' cleanupsRef (cleanup :)

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

whenDockerEnabled :: TestM () -> TestM ()
whenDockerEnabled steps =
  asks (.runConfig.isDockerEnabled) >>= \case
    True -> steps
    False -> logLine "SKIPPED: Docker is disabled via the WASP_E2E_TESTS_SKIP_DOCKER environment variable."
