{-# LANGUAGE NumericUnderscores #-}

module Wasp.Cli.Compile
  ( ensureCompile,
    waitEnsureCompile,
  )
where

import Control.Monad.Except (throwError)
import Data.Functor ((<&>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Compile (compileWithOptions)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..), WaspProcessId, WatcherStatus (..))
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.Message as Msg
import Wasp.Util.IO.Retry (Microseconds, constPause, retryWithCallback)

-- | Makes sure the generated app is up to date with the project's source, for
-- commands that need it.
--   - If no other Wasp command is working on the project, it compiles the
--     project while holding the lock.
--   - If a watcher (e.g. `wasp start`) is keeping the generated app up to date,
--     it trusts it and skips compiling.
--   - If that watcher failed to compile the project, it throws.
--   - If any other Wasp command is working on the project, it throws too. See
--     'waitEnsureCompile' for a version that waits for it to finish instead.
--
-- It doesn't hold the project lock afterwards, so the command can run next to
-- other Wasp commands.
ensureCompile :: CompileOptions -> Command ()
ensureCompile options =
  attemptEnsureCompile options >>= \case
    GeneratedAppUpToDate -> return ()
    ProjectBusy maybeProcessId -> throwError $ makeProjectBusyError maybeProcessId

-- | Like 'ensureCompile', but if another Wasp command is working on the
-- project, it waits for it to finish and checks again.
waitEnsureCompile :: CompileOptions -> Command ()
waitEnsureCompile options =
  retryWithCallback (constPause oneSecond) maxNumRetries printWaiting attempt
    >>= either (throwError . makeProjectBusyError) return
  where
    attempt =
      attemptEnsureCompile options <&> \case
        GeneratedAppUpToDate -> Right ()
        ProjectBusy maybeProcessId -> Left maybeProcessId

    printWaiting numFailedTries _ =
      cliSendMessageC $
        Msg.Start $
          "Waiting for compilation to finish... ("
            ++ show numFailedTries
            ++ "/"
            ++ show maxNumRetries
            ++ ")"

    maxNumRetries = 10
    oneSecond = 1_000_000 :: Microseconds

makeProjectBusyError :: Maybe WaspProcessId -> CommandError
makeProjectBusyError maybeProcessId =
  CommandError "Wasp project is busy" $
    "Another Wasp command"
      ++ maybe "" (\pid -> " (PID " ++ show pid ++ ")") maybeProcessId
      ++ " is working on this project right now. Run this command again once it finishes."

data EnsureCompileResult
  = GeneratedAppUpToDate
  | ProjectBusy (Maybe WaspProcessId)

attemptEnsureCompile :: CompileOptions -> Command EnsureCompileResult
attemptEnsureCompile options =
  ProjectLock.acquire Nothing $ \case
    Right _ -> GeneratedAppUpToDate <$ compileWithOptions options
    Left (Just ProjectLockData {pid, watcherStatus = Just UpToDate}) -> do
      cliSendMessageC $ makeCompilationSkippedInfo pid
      return GeneratedAppUpToDate
    Left (Just ProjectLockData {pid, watcherStatus = Just CompilationFailed}) ->
      throwError $ makeCompilationFailedError pid
    Left maybeProjectLockData -> return $ ProjectBusy $ (.pid) <$> maybeProjectLockData
  where
    makeCompilationSkippedInfo pid =
      Msg.Info $
        "Skipping compilation, another Wasp command (PID "
          ++ show pid
          ++ ") is already watching this project."

    makeCompilationFailedError pid =
      CommandError "Wasp project failed to compile" $
        "Another Wasp command (PID "
          ++ show pid
          ++ ") is running for this project, but it failed to compile it. Fix its compilation errors first."
