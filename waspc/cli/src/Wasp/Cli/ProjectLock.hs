module Wasp.Cli.ProjectLock
  ( withProjectLock,
    WaspProcessId,
    WaspProjectLockfile,
    projectLockFileInWaspProjectDir,
  )
where

import Control.Exception (IOException, try)
import Control.Monad.Catch (bracket)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Lukko
import StrongPath (Abs, Dir, File, Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.IO (Handle, IOMode (ReadWriteMode), hClose, hFlush, hPutStr, hSetFileSize, openFile)
import System.Process (getCurrentPid)
import Text.Read (readMaybe)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir)
import qualified Wasp.Util.IO as Wasp.IO

-- | This file has some information about any process currently running in the
-- project, and is protected by a OS advisory lock to avoid multiple processes
-- working at the same time.
data WaspProjectLockfile

projectLockFileInWaspProjectDir :: Path' (Rel WaspProjectDir) (File WaspProjectLockfile)
projectLockFileInWaspProjectDir = dotWaspDirInWaspProjectDir </> [relfile|.projectlock|]

type WaspProcessId = Integer

-- | Runs the given action while holding an exclusive lock on the Wasp project
-- the current working directory is part of, so no other Wasp process can work
-- on the project at the same time. Throws a 'CommandError' if another process
-- already holds the lock.
withProjectLock :: Command a -> Command a
withProjectLock action = do
  InWaspProject waspProjectDir <- require

  bracket
    (acquireProjectLockOrThrow waspProjectDir)
    (liftIO . releaseProjectLock)
    (const action)
  where
    acquireProjectLockOrThrow waspProjectDir =
      liftIO (acquireProjectLock waspProjectDir) >>= \case
        Right lockFileHandle -> return lockFileHandle
        Left maybeProcessId ->
          throwError $ makeLockedProjectError maybeProcessId

    makeLockedProjectError maybeProcessId =
      CommandError "Wasp project is already in use" $
        "Another Wasp command"
          ++ maybe "" (\pid -> " (PID " ++ show pid ++ ")") maybeProcessId
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
-- On success, writes our PID into the lock file, purely as information for the
-- error message other processes show, and returns the open handle backing the
-- lock so we can hold onto it.
--
-- NOTE: By common convention, the lock file is intentionally **never deleted**,
-- even when the lock is released. This avoids subtle race conditions enabled by
-- POSIX's file handle semantics. See
-- https://theworld.com/~swmcd/steven/tech/flock.html#:~:text=DON%27T%20unlink
-- for an example.
acquireProjectLock :: Path' Abs (Dir WaspProjectDir) -> IO (Either (Maybe WaspProcessId) Handle)
acquireProjectLock waspProjectDir = do
  Directory.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent lockFilePath
  lockFileHandle <- openFile (SP.fromAbsFile lockFilePath) ReadWriteMode
  Lukko.hTryLock lockFileHandle Lukko.ExclusiveLock >>= \case
    True -> do
      writeOwnerProcessId lockFileHandle
      return $ Right lockFileHandle
    False -> do
      hClose lockFileHandle
      Left <$> readOwnerProcessId
  where
    lockFilePath = waspProjectDir </> projectLockFileInWaspProjectDir

    writeOwnerProcessId lockFileHandle = do
      processId <- getCurrentPid
      hSetFileSize lockFileHandle 0
      hPutStr lockFileHandle $ show (fromIntegral processId :: WaspProcessId)
      hFlush lockFileHandle

    readOwnerProcessId :: IO (Maybe WaspProcessId)
    readOwnerProcessId =
      try (Wasp.IO.readFileStrict lockFilePath) >>= \case
        Left (_ :: IOException) -> return Nothing
        Right contents -> return $ readMaybe $ T.unpack $ T.strip contents

releaseProjectLock :: Handle -> IO ()
releaseProjectLock lockFileHandle = do
  Lukko.hUnlock lockFileHandle
  hClose lockFileHandle
