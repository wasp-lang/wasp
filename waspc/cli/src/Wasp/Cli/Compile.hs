module Wasp.Cli.Compile
  ( ensureCompile,
    waitEnsureCompile,
  )
where

import Control.Monad.Except (throwError)
import Data.Functor ((<&>))
import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (analyzeWithOptions, compileWithOptions)
import Wasp.Cli.Command.Install (waspInstallIfNeeded)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..), WaspProcessId, WatcherStatus (..))
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Message as Msg
import Wasp.Util (secondsToMicroSeconds)
import Wasp.Util.IO.Retry (constPause, retryWithCallback)

-- | Makes sure the generated app is up to date with the project's source, for
-- commands that need it.
--   - If no other Wasp command is working on the project, it installs missing
--     dependencies and compiles the project while holding the lock.
--   - If a watcher (e.g. `wasp start`) is keeping the generated app up to date,
--     it trusts it and only analyzes the project to get the 'AS.AppSpec'.
--     Watchers fully set up the generated app, which covers any setup goal.
--   - If that watcher failed to compile the project, it throws.
--   - If any other Wasp command is working on the project, it throws too. See
--     'waitEnsureCompile' for a version that waits for it to finish instead.
--
-- It doesn't hold the project lock afterwards, so the command can run next to
-- other Wasp commands.
ensureCompile :: CompileOptions -> Command AS.AppSpec
ensureCompile options =
  attemptEnsureCompile options >>= \case
    GeneratedAppUpToDate appSpec -> return appSpec
    ProjectBusy maybeProcessId -> throwError $ makeProjectBusyError maybeProcessId

-- | Like 'ensureCompile', but if another Wasp command is working on the
-- project, it waits for it to finish and checks again.
waitEnsureCompile :: CompileOptions -> Command AS.AppSpec
waitEnsureCompile options =
  retryWithCallback (constPause oneSecond) maxNumRetries printWaiting attempt
    >>= either (throwError . makeProjectBusyError) return
  where
    attempt =
      attemptEnsureCompile options <&> \case
        GeneratedAppUpToDate appSpec -> Right appSpec
        ProjectBusy maybeProcessId -> Left maybeProcessId

    printWaiting numFailedTries _ =
      cliSendMessageC
        $ Msg.Start
        $ "Waiting for compilation to finish... ("
          ++ show numFailedTries
          ++ "/"
          ++ show maxNumRetries
          ++ ")"

    maxNumRetries = 10
    oneSecond = fromIntegral $ secondsToMicroSeconds 1

makeProjectBusyError :: Maybe WaspProcessId -> CommandError
makeProjectBusyError maybeProcessId =
  CommandError "Wasp project is busy" $
    ProjectLock.describeLockHolder maybeProcessId
      ++ " is working on this project right now. Run this command again once it finishes."

data EnsureCompileResult
  = GeneratedAppUpToDate AS.AppSpec
  | ProjectBusy (Maybe WaspProcessId)

attemptEnsureCompile :: CompileOptions -> Command EnsureCompileResult
attemptEnsureCompile options =
  ProjectLock.acquire Nothing $ \case
    Right _ -> do
      waspInstallIfNeeded
      WaspSpecAvailable <- require
      GeneratedAppUpToDate . snd <$> compileWithOptions options
    Left (Just ProjectLockData {pid, watcherStatus = Just UpToDate}) -> do
      cliSendMessageC $ makeCompilationSkippedInfo pid
      WaspSpecAvailable <- require
      GeneratedAppUpToDate <$> analyzeWithOptions options.waspProjectDir options
    Left (Just ProjectLockData {pid, watcherStatus = Just CompilationFailed}) ->
      throwError $ makeCompilationFailedError pid
    Left maybeProjectLockData -> return $ ProjectBusy $ (.pid) <$> maybeProjectLockData
  where
    makeCompilationSkippedInfo pid =
      Msg.Info $
        ProjectLock.describeLockHolder (Just pid)
          ++ " is already watching this project, skipping compilation."

    makeCompilationFailedError pid =
      CommandError "Wasp project failed to compile" $
        ProjectLock.describeLockHolder (Just pid)
          ++ " is running for this project, but it failed to compile it. Fix its compilation errors first."
