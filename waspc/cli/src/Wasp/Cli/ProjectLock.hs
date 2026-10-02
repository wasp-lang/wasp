{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.ProjectLock
  ( withProjectLock,
    withProjectLockAsWatcher,
    withProjectLockOrAlongsideWatcher,
    ProjectLock,
    ProjectAccess (..),
    WatcherStatus (..),
    setWatcherStatus,
    WaspProcessId,
    WaspProjectLockfile,
    projectLockFileInWaspProjectDir,
  )
where

import Control.Exception (IOException, try)
import Control.Monad.Catch (bracket)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)
import qualified Lukko
import StrongPath (Abs, Dir, File, Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.IO (Handle, IOMode (ReadWriteMode), SeekMode (AbsoluteSeek), hClose, hFlush, hSeek, hSetFileSize, openFile)
import System.Process (getCurrentPid)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir)

-- | This file has some information about any process currently running in the
-- project, and is protected by a OS advisory lock to avoid multiple processes
-- working at the same time.
data WaspProjectLockfile

projectLockFileInWaspProjectDir :: Path' (Rel WaspProjectDir) (File WaspProjectLockfile)
projectLockFileInWaspProjectDir = dotWaspDirInWaspProjectDir </> [relfile|.projectlock|]

type WaspProcessId = Integer

-- | The project lock, as held by the current process.
newtype ProjectLock = ProjectLock Handle

-- | What the process holding the project lock writes into the lock file, as
-- information for the other processes that find the project locked.
data ProjectLockOwner = ProjectLockOwner
  { pid :: WaspProcessId,
    -- | Only owners that watch the project for changes and recompile on them
    -- (e.g. `wasp start`) have a status. It is 'Nothing' for all the others.
    watcherStatus :: Maybe WatcherStatus
  }
  deriving (Generic, FromJSON, ToJSON)

-- | What a process that watches the project for changes and recompiles on them
-- says about the state of the generated app right now.
data WatcherStatus
  = -- | It is compiling the project, so the generated app is being rewritten.
    Compiling
  | -- | The generated app is up to date with the project's source.
    UpToDate
  | -- | The last compilation failed, so the generated app is outdated until the
    -- user fixes the errors.
    CompilationFailed
  deriving (Generic, FromJSON, ToJSON)

-- | How a command got to work on the project.
data ProjectAccess
  = -- | We hold the project lock, so no other Wasp process is working on the
    -- project.
    ExclusiveProjectAccess
  | -- | Another process holds the project lock, but it is a watcher that has
    -- the generated app up to date, so we can use the generated app as it is.
    -- We must not compile the project ourselves.
    ProjectAccessAlongsideWatcher WaspProcessId

-- | Runs the given action while holding an exclusive lock on the Wasp project
-- the current working directory is part of, so no other Wasp process can work
-- on the project at the same time. Throws a 'CommandError' if another process
-- already holds the lock.
withProjectLock :: Command a -> Command a
withProjectLock = withProjectLockOr Nothing (throwError . makeLockedProjectError) . const

-- | Like 'withProjectLock', but meant for commands that watch the project for
-- changes and recompile on them. They are expected to start by compiling the
-- project, so they start as 'Compiling', and they get the 'ProjectLock' so they
-- can keep other processes posted from then on, via 'setWatcherStatus'.
withProjectLockAsWatcher :: (ProjectLock -> Command a) -> Command a
withProjectLockAsWatcher = withProjectLockOr (Just Compiling) (throwError . makeLockedProjectError)

-- | Like 'withProjectLock', but if the lock is held by a watcher that has the
-- generated app up to date, it runs the action anyway, without the lock,
-- instead of throwing. Meant for commands that only need an up to date
-- generated app, so they can run next to e.g. `wasp start`.
withProjectLockOrAlongsideWatcher :: (ProjectAccess -> Command a) -> Command a
withProjectLockOrAlongsideWatcher action =
  withProjectLockOr
    Nothing
    ( \case
        Just ProjectLockOwner {pid, watcherStatus = Just watcherStatus} -> case watcherStatus of
          UpToDate -> action $ ProjectAccessAlongsideWatcher pid
          Compiling ->
            throwError $
              CommandError "Wasp project is being compiled" $
                "Another Wasp command (PID "
                  ++ show pid
                  ++ ") is compiling this project right now. Run this command again once it finishes."
          CompilationFailed ->
            throwError $
              CommandError "Wasp project failed to compile" $
                "Another Wasp command (PID "
                  ++ show pid
                  ++ ") is running for this project, but it failed to compile it."
                  ++ " Fix the compilation errors it reported, and run this command again once it compiles the project successfully."
        maybeOwner -> throwError $ makeLockedProjectError maybeOwner
    )
    (const $ action ExclusiveProjectAccess)

withProjectLockOr ::
  Maybe WatcherStatus ->
  (Maybe ProjectLockOwner -> Command a) ->
  (ProjectLock -> Command a) ->
  Command a
withProjectLockOr initialWatcherStatus onLockedByAnotherProcess action = do
  InWaspProject waspProjectDir <- require

  bracket
    (liftIO $ acquireProjectLock waspProjectDir initialWatcherStatus)
    (liftIO . mapM_ releaseProjectLock)
    (either onLockedByAnotherProcess action)

makeLockedProjectError :: Maybe ProjectLockOwner -> CommandError
makeLockedProjectError maybeOwner =
  CommandError "Wasp project is already in use" $
    "Another Wasp command"
      ++ maybe "" (\owner -> " (PID " ++ show owner.pid ++ ")") maybeOwner
      ++ " is already running for this project. Stop it before running this command."

-- | Tries to take an exclusive OS-level advisory lock on the project's lock
-- file, creating the file if needed.
--
-- This lock is a kernel-level mechanism, called "advisory" because it is not
-- enforced, it's up to cooperating processes to check for the lock and respect
-- it. The lock is linked to the lifetime of the open file 'Handle', and it is
-- automatically released by the kernel when the process exits (even if it
-- crashes).
--
-- On success, writes our info into the lock file, for other processes that find
-- the project locked to read, and returns the lock.
--
-- NOTE: By common convention, the lock file is intentionally **never deleted**,
-- even when the lock is released. This avoids subtle race conditions enabled by
-- POSIX's file handle semantics. See
-- https://theworld.com/~swmcd/steven/tech/flock.html#:~:text=DON%27T%20unlink
-- for an example.
acquireProjectLock ::
  Path' Abs (Dir WaspProjectDir) ->
  Maybe WatcherStatus ->
  IO (Either (Maybe ProjectLockOwner) ProjectLock)
acquireProjectLock waspProjectDir initialWatcherStatus = do
  Directory.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent lockFilePath
  lockFileHandle <- openFile (SP.fromAbsFile lockFilePath) ReadWriteMode
  Lukko.hTryLock lockFileHandle Lukko.ExclusiveLock >>= \case
    True -> do
      let projectLock = ProjectLock lockFileHandle
      writeOwner projectLock initialWatcherStatus
      return $ Right projectLock
    False -> do
      hClose lockFileHandle
      Left <$> readOwner
  where
    lockFilePath = waspProjectDir </> projectLockFileInWaspProjectDir

    -- NOTE: We treat the owner as unknown if we can't read or parse the file.
    -- That can happen if the owner is in the middle of rewriting it, or on
    -- Windows, where the lock stops other processes from reading the file.
    readOwner :: IO (Maybe ProjectLockOwner)
    readOwner =
      try (B.readFile $ SP.fromAbsFile lockFilePath) >>= \case
        Left (_ :: IOException) -> return Nothing
        Right contents -> return $ Aeson.decodeStrict contents

-- | Tells other processes about the state of the generated app right now, by
-- rewriting our info in the lock file. While it is 'UpToDate', commands that
-- only need an up to date generated app can run next to us (see
-- 'withProjectLockOrAlongsideWatcher').
setWatcherStatus :: ProjectLock -> WatcherStatus -> IO ()
setWatcherStatus projectLock = writeOwner projectLock . Just

writeOwner :: ProjectLock -> Maybe WatcherStatus -> IO ()
writeOwner (ProjectLock lockFileHandle) watcherStatus = do
  processId <- getCurrentPid
  hSetFileSize lockFileHandle 0
  hSeek lockFileHandle AbsoluteSeek 0
  BL.hPut lockFileHandle $
    Aeson.encode ProjectLockOwner {pid = fromIntegral processId, watcherStatus}
  hFlush lockFileHandle

releaseProjectLock :: ProjectLock -> IO ()
releaseProjectLock (ProjectLock lockFileHandle) = do
  Lukko.hUnlock lockFileHandle
  hClose lockFileHandle
