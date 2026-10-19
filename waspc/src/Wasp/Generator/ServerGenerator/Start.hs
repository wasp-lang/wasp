module Wasp.Generator.ServerGenerator.Start
  ( ServerEvent (..),
    CompileOutcome (..),
    CompileChanges (..),
    ServerProcessController,
    newServerProcessController,
    notifyServer,
    startServer,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Void (Void)
import StrongPath (Abs, Dir, File', Path', Rel, (</>))
import System.Exit (ExitCode (..))
import Wasp.Env (addEnvVarsOverride, getEnvVars)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft.Writeable (FileOrDirPathRelativeTo)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import Wasp.Generator.ServerGenerator.ServerEffect (ServerEffect (..), classifyServerEffect)
import qualified Wasp.Job as J
import Wasp.Project.Common (WaspProjectDir)

data ServerProcessController = ServerProcessController
  { -- | The compile waiting to be handled, with an @MVar@ that the controller
    -- fills once it has handled it. Senders wait for that, so there's never
    -- more than one.
    _pendingCompile :: MVar (CompileOutcome, MVar ()),
    -- | Whether the server is running from an up-to-date bundle. Server jobs
    -- set it before acknowledging their compile, so the next one sees it.
    _isServerUpToDate :: TVar Bool,
    -- | Bundles of compiles that started before it are out of date.
    _lastFileChange :: TVar (Maybe UTCTime)
  }

data CompileOutcome
  = SuccessfulCompile CompileChanges
  | FailedCompile

data CompileChanges = CompileChanges
  { -- | Changes from this time on aren't part of the compile.
    _compileStartTime :: UTCTime,
    _changedProjectFiles :: [Path' (Rel WaspProjectDir) File'],
    _changedGeneratedAppPaths :: [FileOrDirPathRelativeTo GeneratedAppDir]
  }

data ServerEvent
  = -- | Interrupts the bundling for any compile that started before the change.
    FilesChanged UTCTime
  | -- | Returns once the controller has handled the compile.
    Compiled CompileOutcome

newServerProcessController :: IO ServerProcessController
newServerProcessController = ServerProcessController <$> newEmptyMVar <*> newTVarIO False <*> newTVarIO Nothing

notifyServer :: ServerProcessController -> ServerEvent -> IO ()
notifyServer controller = \case
  FilesChanged changeTime ->
    atomically $ modifyTVar' (_lastFileChange controller) (max $ Just changeTime)
  Compiled compileOutcome -> do
    done <- newEmptyMVar
    putMVar (_pendingCompile controller) (compileOutcome, done)
    takeMVar done

-- | Bundles and runs the development server, and then keeps it up to date with
-- the compiles the controller is notified about. Never finishes by itself.
startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> J.Job Void
startServer serverRunConfig generatedAppDir controller = do
  startTime <- liftIO getCurrentTime
  initialServerJob <- bundleAndRun startTime noAcknowledgement
  J.keepReplacing nextServerJob $ fromMaybe noServer initialServerJob
  where
    -- Runs alongside the current server job, and returns the one replacing
    -- it, if any.
    nextServerJob :: J.Job (Maybe (J.Job ()))
    nextServerJob = do
      (compileOutcome, done) <- liftIO $ takeMVar $ _pendingCompile controller
      serverUpToDate <- liftIO $ readTVarIO $ _isServerUpToDate controller
      let acknowledge = liftIO $ putMVar done ()
      case (compileOutcome, serverUpToDate) of
        (FailedCompile, _) -> return $ Just $ stopServer acknowledge
        (SuccessfulCompile changes, False) -> announceUpdate >> bundleAndRun (_compileStartTime changes) acknowledge
        (SuccessfulCompile changes, True) -> case serverEffect changes of
          NoChange -> acknowledge $> Nothing
          RestartServer -> announceUpdate $> Just (runServer acknowledge)
          RebundleAndRestartServer -> announceUpdate >> bundleAndRun (_compileStartTime changes) acknowledge

    serverEffect :: CompileChanges -> ServerEffect
    serverEffect changes = classifyServerEffect (_changedProjectFiles changes) (_changedGeneratedAppPaths changes)

    -- If files change while bundling, the bundle is out of date, so it's
    -- interrupted, and the next compile bundles again. The current server
    -- keeps running in the meantime.
    bundleAndRun :: UTCTime -> J.Job () -> J.Job (Maybe (J.Job ()))
    bundleAndRun compileStartTime acknowledge =
      J.race (liftIO $ waitForFileChangeSince compileStartTime) bundle >>= \case
        Right ExitSuccess -> return $ Just $ runServer acknowledge
        Right (ExitFailure _) -> return $ Just $ stopServer acknowledge
        Left () -> do
          liftIO $ setServerUpToDate False
          acknowledge
          return Nothing

    waitForFileChangeSince :: UTCTime -> IO ()
    waitForFileChangeSince time =
      atomically $ readTVar (_lastFileChange controller) >>= check . (>= Just time)

    runServer :: J.Job () -> J.Job ()
    runServer acknowledge = do
      liftIO $ setServerUpToDate True
      acknowledge
      exitCode <- J.onProcessExit (const $ setServerUpToDate False) $ J.fromProc serverProcess
      reportServerExit exitCode
      announceWaitingForChanges

    stopServer :: J.Job () -> J.Job ()
    stopServer acknowledge = do
      liftIO $ setServerUpToDate False
      acknowledge
      announceWaitingForChanges

    setServerUpToDate :: Bool -> IO ()
    setServerUpToDate = atomically . writeTVar (_isServerUpToDate controller)

    noAcknowledgement :: J.Job ()
    noAcknowledgement = return ()

    noServer :: J.Job ()
    noServer = return ()

    announceUpdate :: J.Job ()
    announceUpdate = J.emitOutput J.Stdout "Updating server...\n"

    announceWaitingForChanges :: J.Job ()
    announceWaitingForChanges = J.emitOutput J.Stdout "Waiting for file changes before starting the server...\n"

    -- Same as the `bundle` script in the server's `package.json`.
    bundle :: J.Job ExitCode
    bundle =
      (J.fromProc =<< J.npmBinProc serverDir "tsc" ["--build"])
        `J.andThen` (J.fromProc =<< J.npmBinProc serverDir "rollup" ["--config", "--silent"])

    serverProcess :: J.CreateJobProcess
    serverProcess =
      addEnvVarsOverride (("NODE_ENV", "development") : getEnvVars serverRunConfig)
        $ J.setCwd serverDir
        $ J.proc Common.devServerStartExecutable Common.devServerStartArgs

    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

reportServerExit :: ExitCode -> J.Job ()
reportServerExit = \case
  ExitSuccess -> J.emitOutput J.Stdout "Server process exited.\n"
  ExitFailure code -> J.emitOutput J.Stderr $ T.pack $ "Server process exited with code " <> show code <> ".\n"
