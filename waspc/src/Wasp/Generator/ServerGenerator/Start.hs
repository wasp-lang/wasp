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
import Control.Concurrent.STM (TMVar, TQueue, atomically, newEmptyTMVarIO, newTQueueIO, orElse, putTMVar, readTQueue, takeTMVar, writeTQueue)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import qualified Data.Text as T
import Data.Void (Void, absurd)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), proc)
import Wasp.Env (getEnvVars, inheritEnvWith)
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
  deriving (Eq, Show)

instance Semigroup ServerEffect where
  NoServerEffect <> effect = effect
  effect <> NoServerEffect = effect
  RestartServer <> RestartServer = RestartServer
  _ <> _ = RebundleAndRestartServer

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
        SuccessfulCompile _ done -> bundleAndRun $ Just done
        FailedCompile done -> acknowledge done >> notRunning

    bundleAndRun :: Maybe (MVar ()) -> J.Job Void
    bundleAndRun done =
      bundle >>= \case
        ExitSuccess -> running done
        ExitFailure _ -> mapM_ acknowledge done >> notRunning

    -- Runs the server until it exits, or a command needs it stopped.
    running :: Maybe (MVar ()) -> J.Job Void
    running done = do
      serverExit <- liftIO newEmptyTMVarIO
      next <-
        either absurd id
          <$> J.race
            (runServer serverExit)
            (mapM_ acknowledge done >> handleCommandsWhileRunning serverExit)
      next

    -- Reports the server's exit instead of finishing, so that a command that
    -- is being handled doesn't get cancelled. It reports it as soon as the
    -- server exits, so that a compile right after a crash restarts it.
    runServer :: TMVar ExitCode -> J.Job Void
    runServer serverExit = do
      _ <- J.onProcessExit (atomically . putTMVar serverExit) $ J.fromProc =<< serverProcess
      liftIO $ forever $ threadDelay maxBound

    -- Returns what to do once the server is stopped.
    handleCommandsWhileRunning :: TMVar ExitCode -> J.Job (J.Job Void)
    handleCommandsWhileRunning serverExit =
      liftIO (atomically $ (Left <$> takeTMVar serverExit) `orElse` (Right <$> readTQueue commands)) >>= \case
        Left exitCode -> return $ reportServerExit exitCode >> notRunning
        Right (SuccessfulCompile NoServerEffect done) ->
          acknowledge done >> handleCommandsWhileRunning serverExit
        Right (SuccessfulCompile RestartServer done) -> return $ running $ Just done
        Right (SuccessfulCompile RebundleAndRestartServer done) ->
          -- The current server keeps running while the new one is bundled.
          bundle <&> \case
            ExitSuccess -> running $ Just done
            ExitFailure _ -> acknowledge done >> notRunning
        Right (FailedCompile done) -> return $ acknowledge done >> notRunning

    acknowledge :: MVar () -> J.Job ()
    acknowledge done = liftIO $ putMVar done ()

    bundle :: J.Job ExitCode
    bundle = J.fromProc (proc "npm" ["run", "bundle"]) {cwd = Just serverDir}

    serverProcess :: (MonadIO m) => m CreateProcess
    serverProcess =
      inheritEnvWith
        (("NODE_ENV", "development") : getEnvVars serverRunConfig)
        (proc Common.devServerStartExecutable Common.devServerStartArgs) {cwd = Just serverDir}

    serverDir = SP.fromAbsDir $ generatedAppDir </> Common.serverRootDirInGeneratedAppDir

reportServerExit :: ExitCode -> J.Job ()
reportServerExit = \case
  ExitSuccess -> J.emitOutput J.Stdout "Server process exited.\n"
  ExitFailure code -> J.emitOutput J.Stderr $ T.pack $ "Server process exited with code " <> show code <> ".\n"
