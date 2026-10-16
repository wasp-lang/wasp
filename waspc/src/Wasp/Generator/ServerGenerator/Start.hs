module Wasp.Generator.ServerGenerator.Start
  ( ServerEffect (..),
    ServerProcessController,
    newServerProcessController,
    notifyFailedCompile,
    notifySuccessfulCompile,
    startServer,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.STM (TMVar, TQueue, atomically, newEmptyTMVarIO, newTQueueIO, orElse, putTMVar, readTQueue, takeTMVar, tryTakeTMVar, writeTQueue)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Void (Void, absurd)
import StrongPath (Abs, Dir, Path', (</>))
import System.Exit (ExitCode (..))
import Wasp.Env (addEnvVarsOverride, getEnvVars)
import Wasp.Generator.Common (GeneratedAppDir)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import qualified Wasp.Job as J

newtype ServerProcessController = ServerProcessController (TQueue ServerControllerCommand)

-- Effect of a successful compile on a healthy, running server.
-- Without one, the controller conservatively rebundles before starting.
data ServerEffect
  = NoServerEffect
  | RestartServer
  | RebundleAndRestartServer
  deriving (Eq, Ord, Show)

-- | The stronger of the two effects.
instance Semigroup ServerEffect where
  (<>) = max

instance Monoid ServerEffect where
  mempty = NoServerEffect

-- | Each command carries an @MVar@ that the controller fills once it has
-- handled the command.
data ServerControllerCommand
  = SuccessfulCompile ServerEffect (MVar ())
  | FailedCompile (MVar ())

newServerProcessController :: IO ServerProcessController
newServerProcessController = ServerProcessController <$> newTQueueIO

notifySuccessfulCompile :: ServerProcessController -> ServerEffect -> IO ()
notifySuccessfulCompile controller serverEffect =
  sendCommand controller $ SuccessfulCompile serverEffect

notifyFailedCompile :: ServerProcessController -> IO ()
notifyFailedCompile controller =
  sendCommand controller FailedCompile

-- | Returns once the controller has handled the command.
sendCommand :: ServerProcessController -> (MVar () -> ServerControllerCommand) -> IO ()
sendCommand (ServerProcessController commands) makeCommand = do
  done <- newEmptyMVar
  atomically $ writeTQueue commands $ makeCommand done
  takeMVar done

-- | Bundles and runs the development server, and then keeps it up to date with
-- the compiles the controller is notified about. Never finishes by itself.
startServer :: ServerRunConfig -> Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> J.Job Void
startServer serverRunConfig generatedAppDir (ServerProcessController commands) =
  bundleAndRun Nothing
  where
    -- There is no known-good bundle while the server isn't running, so any
    -- successful compile rebundles it before starting it.
    notRunning :: J.Job Void
    notRunning =
      liftIO (atomically $ readTQueue commands) >>= \case
        SuccessfulCompile _ done -> announceUpdate >> bundleAndRun (Just done)
        FailedCompile done -> acknowledge done >> notRunning

    bundleAndRun :: Maybe (MVar ()) -> J.Job Void
    bundleAndRun done =
      bundle >>= \case
        ExitSuccess -> running done
        ExitFailure _ -> mapM_ acknowledge done >> waitForChanges

    -- Tells the user that the server is down until the next successful compile.
    waitForChanges :: J.Job Void
    waitForChanges = do
      J.emitOutput J.Stdout "Waiting for file changes before starting the server...\n"
      notRunning

    -- Runs the server until it exits, or a command needs it stopped.
    running :: Maybe (MVar ()) -> J.Job Void
    running done = do
      serverExit <- liftIO newEmptyTMVarIO
      either absurd id
        =<< J.race
          (runServer serverExit)
          (mapM_ acknowledge done >> handleCommandsWhileRunning serverExit)

    -- Reports the server's exit instead of finishing, so that a command that
    -- is being handled doesn't get cancelled. It reports it as soon as the
    -- server exits, so that a compile right after a crash restarts it.
    runServer :: TMVar ExitCode -> J.Job Void
    runServer serverExit = do
      _ <- J.onProcessExit (atomically . putTMVar serverExit) $ J.fromProc serverProcess
      liftIO $ forever $ threadDelay maxBound

    -- Returns what to do once the server is stopped.
    handleCommandsWhileRunning :: TMVar ExitCode -> J.Job (J.Job Void)
    handleCommandsWhileRunning serverExit =
      liftIO (atomically $ (Left <$> takeTMVar serverExit) `orElse` (Right <$> readTQueue commands)) >>= \case
        Left exitCode -> return $ reportServerExit exitCode >> waitForChanges
        Right (SuccessfulCompile NoServerEffect done) ->
          acknowledge done >> handleCommandsWhileRunning serverExit
        Right (SuccessfulCompile RestartServer done) -> do
          announceUpdate
          return $ running $ Just done
        Right (SuccessfulCompile RebundleAndRestartServer done) -> do
          announceUpdate
          -- The current server keeps running while the new one is bundled.
          bundleExitCode <- bundle
          -- The server might have exited while it was being bundled.
          mapM_ reportServerExit =<< liftIO (atomically $ tryTakeTMVar serverExit)
          return $ case bundleExitCode of
            ExitSuccess -> running $ Just done
            ExitFailure _ -> acknowledge done >> waitForChanges
        Right (FailedCompile done) -> return $ acknowledge done >> waitForChanges

    acknowledge :: MVar () -> J.Job ()
    acknowledge done = liftIO $ putMVar done ()

    announceUpdate :: J.Job ()
    announceUpdate = J.emitOutput J.Stdout "Updating server...\n"

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
