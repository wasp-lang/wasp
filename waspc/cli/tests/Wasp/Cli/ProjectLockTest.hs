{-# LANGUAGE CPP #-}

module Wasp.Cli.ProjectLockTest where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Maybe (fromJust)
import qualified Lukko
import StrongPath (Abs, Dir, File, Path', (</>))
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.IO.Temp (withSystemTempDirectory)
import System.Process (getCurrentPid)
import Test.Hspec
import Text.Read (readMaybe)
import Wasp.Cli.ProjectLock
  ( ProjectLock,
    ProjectLockError (..),
    WaspProcessId,
    acquireProjectLock,
    releaseProjectLock,
  )
import Wasp.Project.Common
  ( WaspProjectDir,
    WaspProjectLockfile,
    projectLockFileInWaspProjectDir,
  )

spec_projectLock :: Spec
spec_projectLock = do
  describe "project lock file" $ do
    it "rejects an acquire while another process holds the lock" $
      withTempWaspProject $ \waspProjectDir -> do
        writeLockFile waspProjectDir $ show foreignOwnerProcessId
        withForeignLock (lockFilePath waspProjectDir) $
          acquireProjectLock (projectLockFilePath waspProjectDir) >>= \case
            Right _ -> expectationFailure "Expected the lock to be held"
            Left lockError -> lockError `shouldBe` ProjectLockHeld expectedForeignOwner

    it "acquires a lock whose previous owner died" $
      withTempWaspProject $ \waspProjectDir -> do
        -- A leftover lock file without a process holding the OS-level lock is
        -- exactly what a dead owner leaves behind.
        writeLockFile waspProjectDir "999999999"
        releaseProjectLock =<< expectAcquired =<< acquireProjectLock (projectLockFilePath waspProjectDir)

    it "acquires a lock even if the lock file contents are malformed" $
      withTempWaspProject $ \waspProjectDir -> do
        writeLockFile waspProjectDir "not a process ID"
        releaseProjectLock =<< expectAcquired =<< acquireProjectLock (projectLockFilePath waspProjectDir)

    it "writes the current process's PID into the lock file" $
      withTempWaspProject $ \waspProjectDir -> do
        releaseProjectLock =<< expectAcquired =<< acquireProjectLock (projectLockFilePath waspProjectDir)
        expectedProcessId <- fromIntegral <$> getCurrentPid
        contents <- readFile (lockFilePath waspProjectDir)
        readMaybe contents `shouldBe` Just (expectedProcessId :: WaspProcessId)

    it "can be acquired again after release" $
      withTempWaspProject $ \waspProjectDir -> do
        releaseProjectLock =<< expectAcquired =<< acquireProjectLock (projectLockFilePath waspProjectDir)
        releaseProjectLock =<< expectAcquired =<< acquireProjectLock (projectLockFilePath waspProjectDir)

    it "does not delete the lock file on release" $
      withTempWaspProject $ \waspProjectDir -> do
        releaseProjectLock =<< expectAcquired =<< acquireProjectLock (projectLockFilePath waspProjectDir)
        Directory.doesFileExist (lockFilePath waspProjectDir) `shouldReturn` True

foreignOwnerProcessId :: WaspProcessId
foreignOwnerProcessId = 424242

-- | The owner PID that 'acquireProjectLock' is expected to report for a lock
-- held by 'withForeignLock'.
expectedForeignOwner :: Maybe WaspProcessId
#ifdef mingw32_HOST_OS
-- On Windows, a locked file can't be read through other handles, so the
-- owner's PID can't be reported.
expectedForeignOwner = Nothing
#else
expectedForeignOwner = Just foreignOwnerProcessId
#endif

-- | Simulates another process holding the lock. We can't use
-- 'acquireProjectLock' for the holder because GHC forbids opening a file for
-- writing twice within the same process. Lukko's FD API bypasses GHC's handle
-- bookkeeping, so from this process's point of view the lock behaves exactly
-- like one taken by a different process.
withForeignLock :: FilePath -> IO a -> IO a
withForeignLock path action = bracket takeLock dropLock (const action)
  where
    takeLock = do
      fd <- Lukko.fdOpen path
      acquired <- Lukko.fdTryLock fd Lukko.ExclusiveLock
      unless acquired $ expectationFailure "Test setup: expected to take the foreign lock"
      return fd
    dropLock fd = Lukko.fdUnlock fd >> Lukko.fdClose fd

expectAcquired :: Either ProjectLockError ProjectLock -> IO ProjectLock
expectAcquired = \case
  Left lockError -> expectationFailure ("Expected to acquire lock, got: " ++ show lockError) >> error "unreachable"
  Right lock -> return lock

withTempWaspProject :: (Path' Abs (Dir WaspProjectDir) -> IO a) -> IO a
withTempWaspProject action =
  withSystemTempDirectory "wasp-project-lock-test" $ \tempDir -> do
    writeFile (tempDir ++ "/.wasproot") ""
    action $ SP.castDir $ fromJust $ SP.parseAbsDir tempDir

writeLockFile :: Path' Abs (Dir WaspProjectDir) -> String -> IO ()
writeLockFile waspProjectDir contents = do
  Directory.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent $ projectLockFilePath waspProjectDir
  writeFile (lockFilePath waspProjectDir) contents

projectLockFilePath :: Path' Abs (Dir WaspProjectDir) -> Path' Abs (File WaspProjectLockfile)
projectLockFilePath waspProjectDir = waspProjectDir </> projectLockFileInWaspProjectDir

lockFilePath :: Path' Abs (Dir WaspProjectDir) -> FilePath
lockFilePath = SP.fromAbsFile . projectLockFilePath
