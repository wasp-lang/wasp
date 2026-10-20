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
import Data.Functor (($>), (<&>))
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

-- | Bundles and runs the development server, and then keeps it up to date with
-- the compiles the controller is notified about. Never finishes by itself.
startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> J.Job Void
startServer serverRunConfig generatedAppDir controller = do
  startTime <- liftIO getCurrentTime
  (upToDate, initialServerJob) <- bundleAndRun startTime
  liftIO $ setServerUpToDate upToDate
  J.keepReplacing nextServerJob $ fromMaybe noServer initialServerJob
  where
    -- Runs alongside the current server job, and returns the one replacing
    -- it, if any.
    nextServerJob :: J.Job (Maybe (J.Job ()))
    nextServerJob = do
      (compileOutcome, done) <- liftIO $ takeMVar $ _pendingCompile controller
      serverUpToDate <- liftIO $ readTVarIO $ _isServerUpToDate controller
      (upToDate, replacementServerJob) <- handleCompile compileOutcome serverUpToDate
      liftIO $ do
        setServerUpToDate upToDate
        putMVar done ()
      return replacementServerJob

    -- Returns whether the server will be up to date, and the job replacing
    -- the current one, if any.
    handleCompile :: CompileOutcome -> Bool -> J.Job (Bool, Maybe (J.Job ()))
    handleCompile compileOutcome serverUpToDate =
      case (compileOutcome, serverUpToDate) of
        (FailedCompile, _) -> return stoppedServer
        (SuccessfulCompile changes, False) -> printUpdatingServer >> bundleAndRun (_compileStartTime changes)
        (SuccessfulCompile changes, True) -> case serverEffect changes of
          NoChange -> return unchangedServer
          RestartServer -> printUpdatingServer $> runningServer
          RebundleAndRestartServer -> printUpdatingServer >> bundleAndRun (_compileStartTime changes)

    serverEffect :: CompileChanges -> ServerEffect
    serverEffect changes = classifyServerEffect (_changedProjectFiles changes) (_changedGeneratedAppPaths changes)

    -- If files change while bundling, the bundle is out of date, so it's
    -- interrupted, and the next compile bundles again. The current server
    -- keeps running in the meantime.
    bundleAndRun :: UTCTime -> J.Job (Bool, Maybe (J.Job ()))
    bundleAndRun compileStartTime =
      J.race (liftIO $ waitForFileChangeSince compileStartTime) bundle <&> \case
        Right ExitSuccess -> runningServer
        Right (ExitFailure _) -> stoppedServer
        Left () -> outOfDateServer

    waitForFileChangeSince :: UTCTime -> IO ()
    waitForFileChangeSince time =
      atomically $ readTVar (_lastFileChange controller) >>= check . (>= Just time)

    runningServer, stoppedServer, unchangedServer, outOfDateServer :: (Bool, Maybe (J.Job ()))
    runningServer = (True, Just runServer)
    -- Replacing the current job stops the server.
    stoppedServer = (False, Just printWaitingForChanges)
    unchangedServer = (True, Nothing)
    -- The current server keeps running, but the next compile has to bundle it
    -- again.
    outOfDateServer = (False, Nothing)

    runServer :: J.Job ()
    runServer = do
      exitCode <- J.onProcessExit (const $ setServerUpToDate False) $ J.fromProc serverProcess
      reportServerExit exitCode
      printWaitingForChanges

    setServerUpToDate :: Bool -> IO ()
    setServerUpToDate = atomically . writeTVar (_isServerUpToDate controller)

    noServer :: J.Job ()
    noServer = return ()

    printUpdatingServer :: J.Job ()
    printUpdatingServer = J.emitOutput J.Stdout "Updating server...\n"

    printWaitingForChanges :: J.Job ()
    printWaitingForChanges = J.emitOutput J.Stdout "Waiting for file changes before starting the server...\n"

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

reportServerExit :: ExitCode -> J.Job ()
reportServerExit = \case
  ExitSuccess -> J.emitOutput J.Stdout "Server process exited.\n"
  ExitFailure code -> J.emitOutput J.Stderr $ T.pack $ "Server process exited with code " <> show code <> ".\n"

data ServerEvent
  = -- | Interrupts the bundling for any compile that started before the change.
    FilesChanged UTCTime
  | -- | Returns once the controller has handled the compile, which can
    -- include bundling the server. A 'FilesChanged' interrupts the bundling.
    Compiled CompileOutcome

data CompileOutcome
  = SuccessfulCompile CompileChanges
  | FailedCompile

data CompileChanges = CompileChanges
  { -- | Changes from this time on aren't part of the compile.
    _compileStartTime :: UTCTime,
    _changedProjectFiles :: [Path' (Rel WaspProjectDir) File'],
    _changedGeneratedAppPaths :: [FileOrDirPathRelativeTo GeneratedAppDir]
  }

data ServerProcessController = ServerProcessController
  { -- | The compile waiting to be handled, with an @MVar@ that the controller
    -- fills once it has handled it. Senders wait for that, so there's never
    -- more than one.
    _pendingCompile :: MVar (CompileOutcome, MVar ()),
    -- | Whether the server is running from an up-to-date bundle. It's set
    -- before a compile is marked handled, so the next compile sees it.
    _isServerUpToDate :: TVar Bool,
    -- | Bundles of compiles that started before it are out of date.
    _lastFileChange :: TVar (Maybe UTCTime)
  }
