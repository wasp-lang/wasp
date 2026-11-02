module Wasp.Generator.ServerGenerator.Start
  ( ServerEvent (..),
    CompileOutcome (..),
    CompileChanges (..),
    ServerInbox,
    newServerInbox,
    notifyServer,
    waitUntilCompilesHandled,
    startServer,
  )
where

import Control.Concurrent.STM (TVar, atomically, check, modifyTVar', newTVarIO, readTVar, retry, swapTVar, writeTVar)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Void (Void)
import StrongPath (Abs, Dir, File', Path', Rel, (</>))
import System.Exit (ExitCode (..))
import qualified UnliftIO.Async as UnliftIO
import Wasp.Env (addEnvVarsOverride, getEnvVars)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import Wasp.Generator.ServerGenerator.ServerEffect (ServerEffect (..), classifyServerEffect)
import Wasp.Generator.WriteFileDrafts (ChangedGeneratedAppPath)
import qualified Wasp.Job as J
import Wasp.Project.Common (WaspProjectDir)

-- We disable this lint because we use it for clarity here, because it is
-- complex code:
{- HLINT ignore startServer "Redundant return" -}

-- | Bundles and runs the development server, and then keeps it up to date with
-- the compiles sent to the inbox. Never finishes by itself.
startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> ServerInbox -> J.Job Void
startServer serverRunConfig generatedAppDir inbox = do
  startTime <- liftIO getCurrentTime
  join $ bundleAndRunServer startTime $ return $ runUntilReplaced noServer False
  where
    -- Runs the server job, and handles compiles until it has to be replaced.
    -- `withAsync` waits for the server job to stop, and the server process to
    -- exit with it, before the next job starts, so that the port is free.
    runUntilReplaced :: J.Job () -> Bool -> J.Job Void
    runUntilReplaced currentServerJob isBundleUpToDate =
      join $ UnliftIO.withAsync currentServerJob $ \server -> do
        UnliftIO.link server -- Rethrows the server job's exception.
        nextServerJob <- produceNextServerJob server isBundleUpToDate
        return nextServerJob

    -- Handles compiles while the server job keeps running. Returns the `Job` to
    -- switch to.
    produceNextServerJob :: UnliftIO.Async () -> Bool -> J.Job (J.Job Void)
    produceNextServerJob server isBundleUpToDate = do
      serverUpdate <- takeServerUpdate

      isServerRunning <- isNothing <$> UnliftIO.poll server
      let hasUpToDateServer = isServerRunning && isBundleUpToDate

      case (hasUpToDateServer, serverUpdate) of
        (_, StopServer) -> waitForChanges
        (False, ApplyServerEffect compileStartTime _) -> bundleAndRunServer compileStartTime rebundleOnNextCompile
        (True, ApplyServerEffect compileStartTime RebundleAndRestartServer) -> bundleAndRunServer compileStartTime rebundleOnNextCompile
        (True, ApplyServerEffect _ RestartServer) -> restartServer
        (True, ApplyServerEffect _ NoChange) -> produceNextServerJob server isBundleUpToDate
      where
        rebundleOnNextCompile =
          -- The current server keeps running meanwhile.
          produceNextServerJob server False

    -- No server runs until the next successful compile, which bundles and
    -- starts it.
    waitForChanges :: J.Job (J.Job Void)
    waitForChanges = return $ printWaitingForChanges >> runUntilReplaced noServer False

    restartServer :: J.Job (J.Job Void)
    restartServer = printUpdatingServer >> runServer

    -- If files change while bundling, the bundle is out of date, so it's
    -- interrupted.
    bundleAndRunServer compileStartTime onFileChangeInterruption =
      printUpdatingServer
        >> J.race (liftIO $ waitForFileChangeSince compileStartTime) bundle
        >>= \case
          Right ExitSuccess -> runServer
          Right (ExitFailure _) -> waitForChanges
          Left () -> onFileChangeInterruption

    runServer :: J.Job (J.Job Void)
    runServer = return $ runUntilReplaced runServerProcess True

    noServer :: J.Job ()
    noServer = return ()

    -- Waits for compiles that `startServer` hasn't handled yet, and returns
    -- what they require of the server.
    takeServerUpdate :: J.Job ServerUpdate
    takeServerUpdate = liftIO $ do
      atomically $ writeTVar (_isWaitingForCompiles inbox) True
      atomically $
        swapTVar (_pendingCompiles inbox) [] >>= \pendingCompiles -> case nonEmpty pendingCompiles of
          Nothing -> retry
          Just compiles -> do
            writeTVar (_isWaitingForCompiles inbox) False
            return $ serverUpdateFor compiles

    waitForFileChangeSince :: UTCTime -> IO ()
    waitForFileChangeSince time =
      atomically $ readTVar (_lastFileChange inbox) >>= check . (>= Just time)

    runServerProcess :: J.Job ()
    runServerProcess =
      J.fromProc serverProcess
        >>= reportServerExit
        >> printWaitingForChanges

    printUpdatingServer :: J.Job ()
    printUpdatingServer =
      J.emitOutput J.Stdout "Updating server...\n"

    printWaitingForChanges :: J.Job ()
    printWaitingForChanges =
      J.emitOutput J.Stdout "Waiting for file changes before starting the server...\n"

    bundle :: J.Job ExitCode
    bundle =
      (J.fromProc =<< uncurry (J.npmBinProc serverDir) Common.serverTscProcess)
        `J.andThen` (J.fromProc =<< uncurry (J.npmBinProc serverDir) Common.serverRollupProcess)

    serverProcess :: J.CreateJobProcess
    serverProcess =
      addEnvVarsOverride (("NODE_ENV", "development") : getEnvVars serverRunConfig)
        $ J.setCwd serverDir
        $ uncurry J.proc Common.devServerStartProcess

    serverDir = generatedAppDir </> Common.serverRootDirInGeneratedAppDir

-- | What the compiles that `startServer` hasn't handled yet, newest first,
-- require of the server.
serverUpdateFor :: NonEmpty CompileOutcome -> ServerUpdate
serverUpdateFor compiles@(latestCompile :| _) = case latestCompile of
  FailedCompile -> StopServer
  SuccessfulCompile changes -> ApplyServerEffect (_compileStartTime changes) $ maximum $ compileServerEffect <$> compiles
  where
    compileServerEffect :: CompileOutcome -> ServerEffect
    compileServerEffect = \case
      -- Handled on its own, a failed compile would have stopped the server, and
      -- the next successful one would have bundled it again.
      FailedCompile -> RebundleAndRestartServer
      SuccessfulCompile changes -> classifyServerEffect (_changedProjectFiles changes) (_changedGeneratedAppPaths changes)

newServerInbox :: IO ServerInbox
newServerInbox = ServerInbox <$> newTVarIO [] <*> newTVarIO Nothing <*> newTVarIO False

notifyServer :: ServerInbox -> ServerEvent -> IO ()
notifyServer inbox = \case
  FilesChanged changeTime ->
    atomically $ modifyTVar' (_lastFileChange inbox) (max $ Just changeTime)
  Compiled compileOutcome ->
    atomically $ modifyTVar' (_pendingCompiles inbox) (compileOutcome :)

-- | Returns once `startServer` has handled all the compiles it was notified
-- about, and is waiting for more. For tests, which otherwise couldn't tell when
-- a compile that changes nothing has been handled.
waitUntilCompilesHandled :: ServerInbox -> IO ()
waitUntilCompilesHandled inbox = atomically $ do
  isWaitingForCompiles <- readTVar $ _isWaitingForCompiles inbox
  pendingCompiles <- readTVar $ _pendingCompiles inbox
  check $ isWaitingForCompiles && null pendingCompiles

reportServerExit :: ExitCode -> J.Job ()
reportServerExit = \case
  ExitSuccess -> J.emitOutput J.Stdout "Server process exited.\n"
  ExitFailure code -> J.emitOutput J.Stderr $ T.pack $ "Server process exited with code " <> show code <> ".\n"

data ServerEvent
  = -- | Interrupts the bundling for any compile that started before the change.
    FilesChanged UTCTime
  | -- | Queues the compile for `startServer`, which handles it, together
    -- with any others queued in the meantime, once it's done bundling.
    Compiled CompileOutcome

data CompileOutcome
  = SuccessfulCompile CompileChanges
  | FailedCompile

data CompileChanges = CompileChanges
  { -- | Changes from this time on aren't part of the compile.
    _compileStartTime :: UTCTime,
    _changedProjectFiles :: [Path' (Rel WaspProjectDir) File'],
    _changedGeneratedAppPaths :: [ChangedGeneratedAppPath]
  }

data ServerUpdate
  = StopServer
  | -- | With the start time of the latest compile, and the strongest effect
    -- among the compiles.
    ApplyServerEffect UTCTime ServerEffect

data ServerInbox = ServerInbox
  { -- | The compiles that `startServer` hasn't handled yet, newest first.
    _pendingCompiles :: TVar [CompileOutcome],
    -- | Bundles of compiles that started before it are out of date.
    _lastFileChange :: TVar (Maybe UTCTime),
    -- | Whether `startServer` is waiting for compiles, having handled the
    -- previous ones.
    _isWaitingForCompiles :: TVar Bool
  }
