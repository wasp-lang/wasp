module Wasp.Cli.ProjectLock
  ( acquireExclusive,
    acquireWatcher,
    acquire,
    WaspProcessId,
    WaspProjectLockfile,
    projectLockFileInWaspProjectDir,
  )
where

import Control.Monad.Catch (bracket)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..), WaspProcessId, WatcherStatus (..))
import Wasp.Cli.ProjectLock.Handle
  ( ProjectLock,
    WaspProjectLockfile,
    acquireProjectLock,
    projectLockFileInWaspProjectDir,
    releaseProjectLock,
  )

-- | Runs the given action while holding an exclusive lock, so no other Wasp
-- process can run at the same time. Throws a 'CommandError' if another process
-- already holds the lock.
acquireExclusive :: Command a -> Command a
acquireExclusive action =
  acquire Nothing $ \case
    Left maybeProjectLockData -> throwError $ makeLockedProjectError maybeProjectLockData
    Right _ -> action

-- | Meant for commands that watch the project for changes and recompile on them
-- (e.g. `wasp start`). They are expected to start by compiling the project, so
-- they start as 'Compiling', and they get the 'ProjectLock' so they can keep
-- other processes posted from then on, via
-- 'Wasp.Cli.ProjectLock.Handle.setWatcherStatus'.
acquireWatcher :: (ProjectLock -> Command a) -> Command a
acquireWatcher action =
  acquire (Just Compiling) $ \case
    Left maybeProjectLockData -> throwError $ makeLockedProjectError maybeProjectLockData
    Right projectLock -> action projectLock

-- | Tries to take the project lock, with the given watcher status in the lock
-- file, and runs the given action with the result: the 'ProjectLock' if we got
-- it, or what the process holding it wrote into the lock file if we didn't. If
-- we got the lock, we hold it until the action finishes.
acquire ::
  Maybe WatcherStatus ->
  (Either (Maybe ProjectLockData) ProjectLock -> Command b) ->
  Command b
acquire initialWatcherStatus action = do
  InWaspProject waspProjectDir <- require

  bracket
    (liftIO $ acquireProjectLock waspProjectDir initialWatcherStatus)
    (liftIO . mapM_ releaseProjectLock)
    action

makeLockedProjectError :: Maybe ProjectLockData -> CommandError
makeLockedProjectError maybeProjectLockData =
  CommandError "Wasp project is already in use" $
    "Another Wasp command"
      ++ maybe "" (\projectLockData -> " (PID " ++ show projectLockData.pid ++ ")") maybeProjectLockData
      ++ " is already running for this project. Wait for it, or stop it and try again."
