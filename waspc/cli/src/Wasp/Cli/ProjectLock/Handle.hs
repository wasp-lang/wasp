module Wasp.Cli.ProjectLock.Handle
  ( ProjectLock,
    acquireProjectLock,
    releaseProjectLock,
    setWatcherStatus,
    WaspProjectLockfile,
    projectLockFileInWaspProjectDir,
  )
where

import Control.Exception (IOException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Lukko
import StrongPath (Abs, Dir, File, Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.IO (Handle, IOMode (ReadWriteMode), SeekMode (AbsoluteSeek), hClose, hFlush, hSeek, hSetFileSize, openFile)
import System.Process (getCurrentPid)
import Wasp.Cli.ProjectLock.Data (ProjectLockData (..), WatcherStatus)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir)

-- | This file has some information about any process currently running in the
-- project, and is protected by a OS advisory lock to avoid multiple processes
-- working at the same time.
data WaspProjectLockfile

projectLockFileInWaspProjectDir :: Path' (Rel WaspProjectDir) (File WaspProjectLockfile)
projectLockFileInWaspProjectDir = dotWaspDirInWaspProjectDir </> [relfile|.projectlock|]

-- | The project lock, as held by the current process.
newtype ProjectLock = ProjectLock Handle

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
  IO (Either (Maybe ProjectLockData) ProjectLock)
acquireProjectLock waspProjectDir initialWatcherStatus = do
  Directory.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent lockFilePath
  lockFileHandle <- openFile (SP.fromAbsFile lockFilePath) ReadWriteMode
  Lukko.hTryLock lockFileHandle Lukko.ExclusiveLock >>= \case
    True -> do
      let projectLock = ProjectLock lockFileHandle
      writeProjectLockData projectLock initialWatcherStatus
      return $ Right projectLock
    False -> do
      hClose lockFileHandle
      Left <$> readProjectLockData
  where
    lockFilePath = waspProjectDir </> projectLockFileInWaspProjectDir

    -- NOTE: We treat the data as unknown if we can't read or parse the file.
    -- That can happen if the owner is in the middle of rewriting it, or on
    -- Windows, where the lock stops other processes from reading the file.
    readProjectLockData :: IO (Maybe ProjectLockData)
    readProjectLockData =
      try (B.readFile $ SP.fromAbsFile lockFilePath) >>= \case
        Left (_ :: IOException) -> return Nothing
        Right contents -> return $ Aeson.decodeStrict contents

-- | Tells other processes about the state of the generated app right now, by
-- rewriting our info in the lock file.
setWatcherStatus :: ProjectLock -> WatcherStatus -> IO ()
setWatcherStatus projectLock = writeProjectLockData projectLock . Just

writeProjectLockData :: ProjectLock -> Maybe WatcherStatus -> IO ()
writeProjectLockData (ProjectLock lockFileHandle) watcherStatus = do
  processId <- getCurrentPid
  hSetFileSize lockFileHandle 0
  hSeek lockFileHandle AbsoluteSeek 0
  BL.hPut lockFileHandle $
    Aeson.encode $
      ProjectLockData {pid = fromIntegral processId, watcherStatus}
  hFlush lockFileHandle

releaseProjectLock :: ProjectLock -> IO ()
releaseProjectLock (ProjectLock lockFileHandle) = do
  Lukko.hUnlock lockFileHandle
  hClose lockFileHandle
