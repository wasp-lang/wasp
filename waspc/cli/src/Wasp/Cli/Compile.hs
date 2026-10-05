module Wasp.Cli.Compile
  ( ensureCompile,
    waitEnsureCompile,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified System.Info
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Compile (compileWithOptions)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..), WaspProcessId, WatcherStatus (..))
import Wasp.CompileOptions (CompileOptions)
import qualified Wasp.Message as Msg

-- | Makes sure the generated app is up to date with the project's source, for
-- commands that only need to use it. It doesn't hold the project lock
-- afterwards, so the command can run next to other Wasp commands:
--   - If no other Wasp command is working on the project, it compiles the
--     project while holding the lock.
--   - If a watcher (e.g. `wasp start`) is keeping the generated app up to date,
--     it trusts it and skips compiling.
--   - If that watcher failed to compile the project, it throws.
--   - If any other Wasp command is working on the project, it throws too. See
--     'waitEnsureCompile' for a version that waits for it to finish instead.
ensureCompile :: CompileOptions -> Command ()
ensureCompile options =
  tryEnsureCompile options >>= \case
    GeneratedAppUpToDate -> return ()
    ProjectBusy maybeProcessId -> throwError $ makeProjectBusyError maybeProcessId

-- | Like 'ensureCompile', but if another Wasp command is working on the
-- project, it waits a bit for it to finish before giving up.
--
-- NOTE: On Windows, it doesn't wait. There, the lock stops other processes
-- from reading the lock file, so we can never tell what the lock holder is
-- doing, and waiting would only delay the same error.
waitEnsureCompile :: CompileOptions -> Command ()
waitEnsureCompile options
  | System.Info.os == "mingw32" = ensureCompile options
  | otherwise = attempt maxRetries
  where
    attempt retriesLeft =
      tryEnsureCompile options >>= \case
        GeneratedAppUpToDate -> return ()
        ProjectBusy maybeProcessId
          | retriesLeft <= 0 -> throwError $ makeProjectBusyError maybeProcessId
          | otherwise -> do
              when (retriesLeft == maxRetries) $
                cliSendMessageC $
                  Msg.Start "Waiting for compilation to finish..."
              liftIO $ threadDelay retryDelayInMicroseconds
              attempt $ retriesLeft - 1

    maxRetries = 10 :: Int
    retryDelayInMicroseconds = 1000000

data EnsureCompileResult
  = -- | The generated app is up to date with the project's source.
    GeneratedAppUpToDate
  | -- | Another Wasp command is working on the project, so the generated app
    -- may change under us. Trying again once it finishes may work.
    ProjectBusy (Maybe WaspProcessId)

tryEnsureCompile :: CompileOptions -> Command EnsureCompileResult
tryEnsureCompile options =
  ProjectLock.acquire Nothing $ \case
    Right _ -> GeneratedAppUpToDate <$ compileWithOptions options
    Left (Just ProjectLockData {pid, watcherStatus = Just UpToDate}) -> do
      cliSendMessageC $
        Msg.Info $
          "Skipping compilation, another Wasp command (PID "
            ++ show pid
            ++ ") is already keeping this project compiled."
      return GeneratedAppUpToDate
    Left (Just ProjectLockData {pid, watcherStatus = Just CompilationFailed}) ->
      throwError $
        CommandError "Wasp project failed to compile" $
          "Another Wasp command (PID "
            ++ show pid
            ++ ") is running for this project, but it failed to compile it."
            ++ " Fix the compilation errors it reported, and run this command again once it compiles the project successfully."
    Left maybeProjectLockData -> return $ ProjectBusy $ (.pid) <$> maybeProjectLockData

makeProjectBusyError :: Maybe WaspProcessId -> CommandError
makeProjectBusyError maybeProcessId =
  CommandError "Wasp project is busy" $
    "Another Wasp command"
      ++ maybe "" (\pid -> " (PID " ++ show pid ++ ")") maybeProcessId
      ++ " is working on this project right now. Run this command again once it finishes."
