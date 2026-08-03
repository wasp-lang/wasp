module Wasp.Cli.ProjectLock
  ( ProjectLockError (..),
    WaspProcessId,
    WaspProjectLockfile,
    projectLockFileInWaspProjectDir,
    acquireProjectLock,
    releaseProjectLock,
  )
where

import Control.Exception (IOException, try)
import qualified Data.Text as T
import qualified Lukko
import StrongPath (Abs, File, Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.IO (Handle, IOMode (ReadWriteMode), hClose, hFlush, hPutStr, hSetFileSize, openFile)
import System.Process (getCurrentPid)
import Text.Read (readMaybe)
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir)
import qualified Wasp.Util.IO as Wasp.IO

type WaspProcessId = Integer

-- | This file has some information about any process currently running in the
-- project, and is protected by a OS advisory lock to avoid multiple processes
-- working at the same time.
data WaspProjectLockfile

projectLockFileInWaspProjectDir :: Path' (Rel WaspProjectDir) (File WaspProjectLockfile)
projectLockFileInWaspProjectDir = dotWaspDirInWaspProjectDir </> [relfile|.lock|]

newtype ProjectLockError
  = -- | Another process holds the lock. Carries that process's PID as read
    -- from the lock file, if it could be read and parsed.
    ProjectLockHeld (Maybe WaspProcessId)
  deriving (Eq, Show)

-- | Tries to take an exclusive OS-level advisory lock on the project's lock
-- file, creating the file if needed. On success, writes our PID into the lock
-- file, purely as information for the error message other processes show, and
-- returns the open handle backing the lock: the lock is held for exactly as
-- long as the handle stays open and locked, so release it by unlocking and
-- closing the handle. The OS releases the lock when the process exits, even
-- if it crashes.
--
-- NOTE: The lock file is intentionally never deleted, not even on release. An
-- advisory lock protects the open file, but not its directory entry: if we
-- deleted the file, another process could re-create the path and lock the new
-- file while a third process still holds the lock on the old, now-unlinked
-- one, leaving two processes convinced they hold the project lock.
-- See https://theworld.com/~swmcd/steven/tech/flock.html for details.
acquireProjectLock :: Path' Abs (File WaspProjectLockfile) -> IO (Either ProjectLockError Handle)
acquireProjectLock lockFilePath = do
  Directory.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent lockFilePath
  lockFileHandle <- openFile (SP.fromAbsFile lockFilePath) ReadWriteMode
  Lukko.hTryLock lockFileHandle Lukko.ExclusiveLock >>= \case
    True -> do
      writeOwnerProcessId lockFileHandle
      return $ Right lockFileHandle
    False -> do
      hClose lockFileHandle
      Left . ProjectLockHeld <$> readOwnerProcessId
  where
    -- We write through the handle holding the lock because, on Windows, a
    -- locked file can't be written to through other handles.
    writeOwnerProcessId lockFileHandle = do
      processId <- getCurrentPid
      hSetFileSize lockFileHandle 0
      hPutStr lockFileHandle $ show (fromIntegral processId :: WaspProcessId)
      hFlush lockFileHandle

    -- Reading can fail (on Windows, a locked file can't be read through other
    -- handles) and parsing can fail (e.g. the owner hasn't written its PID
    -- yet), in which case we just don't know who the owner is.
    readOwnerProcessId :: IO (Maybe WaspProcessId)
    readOwnerProcessId =
      try (Wasp.IO.readFileStrict lockFilePath) >>= \case
        Left (_ :: IOException) -> return Nothing
        Right contents -> return $ readMaybe $ T.unpack $ T.strip contents

releaseProjectLock :: Handle -> IO ()
releaseProjectLock lockFileHandle = do
  Lukko.hUnlock lockFileHandle
  hClose lockFileHandle
