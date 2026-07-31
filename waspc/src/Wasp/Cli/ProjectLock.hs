module Wasp.Cli.ProjectLock
  ( ProjectLock,
    ProjectLockError (..),
    WaspProcessId,
    acquireProjectLock,
    releaseProjectLock,
  )
where

import Control.Exception (IOException, try)
import qualified Data.Text as T
import qualified Lukko
import StrongPath (Abs, File, Path')
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.IO (Handle, IOMode (ReadWriteMode), hClose, hFlush, hPutStr, hSetFileSize, openFile)
import System.Process (getCurrentPid)
import Text.Read (readMaybe)
import Wasp.Project.Common (WaspProjectLockfile)
import qualified Wasp.Util.IO as Wasp.IO

type WaspProcessId = Integer

-- | Proof that this process holds the lock on a Wasp project.
--
-- Holds the open handle backing the OS-level lock: the lock is held for
-- exactly as long as the handle stays open, so keep this value reachable for
-- as long as the project should stay locked (GHC closes handles that become
-- unreachable). The OS releases the lock when the process exits, even if it
-- crashes.
newtype ProjectLock = ProjectLock Handle

newtype ProjectLockError
  = -- | Another process holds the lock. Carries that process's PID as read
    -- from the lock file, if it could be read and parsed.
    ProjectLockHeld (Maybe WaspProcessId)
  deriving (Eq, Show)

-- | Tries to take an exclusive OS-level advisory lock on the project's lock
-- file, creating the file if needed. On success, writes our PID into the lock
-- file, purely as information for the error message other processes show.
--
-- NOTE: The lock file is intentionally never deleted, not even on release. An
-- advisory lock protects the open file, but not its directory entry: if we
-- deleted the file, another process could re-create the path and lock the new
-- file while a third process still holds the lock on the old, now-unlinked
-- one, leaving two processes convinced they hold the project lock.
-- See https://theworld.com/~swmcd/steven/tech/flock.html for details.
acquireProjectLock :: Path' Abs (File WaspProjectLockfile) -> IO (Either ProjectLockError ProjectLock)
acquireProjectLock lockFilePath = do
  Directory.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent lockFilePath
  lockFileHandle <- openFile (SP.fromAbsFile lockFilePath) ReadWriteMode
  Lukko.hTryLock lockFileHandle Lukko.ExclusiveLock >>= \case
    True -> do
      writeOwnerProcessId lockFileHandle
      return $ Right $ ProjectLock lockFileHandle
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

-- | Releases the lock by closing the handle holding it. The lock file itself
-- stays behind; see 'acquireProjectLock' for why it must not be deleted.
releaseProjectLock :: ProjectLock -> IO ()
releaseProjectLock (ProjectLock lockFileHandle) = hClose lockFileHandle
