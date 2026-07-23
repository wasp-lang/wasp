module Wasp.Cli.ProjectLockTest where

import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, File, Path', (</>))
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Wasp.Cli.ProjectLock
  ( ProjectLockError (..),
    acquireProjectLock,
    releaseProjectLock,
  )
import Wasp.Cli.ProjectLock.System (getCurrentWaspProcessId)
import Wasp.Project.Common
  ( WaspProjectDir,
    WaspProjectLockfile,
    projectLockFileInWaspProjectDir,
  )

spec_projectLock :: Spec
spec_projectLock = do
  describe "project lock file" $ do
    it "rejects a lock owned by a running process" $
      withTempWaspProject $ \waspProjectDir -> do
        processId <- getCurrentWaspProcessId
        writeLockFile waspProjectDir $ show processId

        acquireProjectLock (projectLockFilePath waspProjectDir)
          `shouldReturn` Left (ProjectLockHeld processId)

    it "reclaims a lock owned by a dead process" $
      withTempWaspProject $ \waspProjectDir -> do
        writeLockFile waspProjectDir "999999999"

        acquireProjectLock (projectLockFilePath waspProjectDir) >>= \case
          Left lockError -> expectationFailure $ "Expected to acquire lock, got: " ++ show lockError
          Right _ -> releaseProjectLock (projectLockFilePath waspProjectDir)

    it "fails safely when the lock file is malformed" $
      withTempWaspProject $ \waspProjectDir -> do
        writeLockFile waspProjectDir "not a process ID"

        acquireProjectLock (projectLockFilePath waspProjectDir) >>= \case
          Left (ProjectLockMalformed _) -> return ()
          other -> expectationFailure $ "Expected ProjectLockMalformed, got: " ++ show other

    it "can be acquired again after release" $
      withTempWaspProject $ \waspProjectDir -> do
        _ <- expectAcquired =<< acquireProjectLock (projectLockFilePath waspProjectDir)
        releaseProjectLock (projectLockFilePath waspProjectDir)
        _ <- expectAcquired =<< acquireProjectLock (projectLockFilePath waspProjectDir)
        releaseProjectLock (projectLockFilePath waspProjectDir)

expectAcquired :: Either ProjectLockError processId -> IO processId
expectAcquired = \case
  Left lockError -> expectationFailure ("Expected to acquire lock, got: " ++ show lockError) >> error "unreachable"
  Right processId -> return processId

withTempWaspProject :: (Path' Abs (Dir WaspProjectDir) -> IO a) -> IO a
withTempWaspProject action =
  withSystemTempDirectory "wasp-project-lock-test" $ \tempDir -> do
    writeFile (tempDir ++ "/.wasproot") ""
    action $ SP.castDir $ fromJust $ SP.parseAbsDir tempDir

writeLockFile :: Path' Abs (Dir WaspProjectDir) -> String -> IO ()
writeLockFile waspProjectDir contents = do
  let lockFile = lockFilePath waspProjectDir
  Directory.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent $ projectLockFilePath waspProjectDir
  writeFile lockFile contents

projectLockFilePath :: Path' Abs (Dir WaspProjectDir) -> Path' Abs (File WaspProjectLockfile)
projectLockFilePath waspProjectDir = waspProjectDir </> projectLockFileInWaspProjectDir

lockFilePath :: Path' Abs (Dir WaspProjectDir) -> FilePath
lockFilePath = SP.fromAbsFile . projectLockFilePath
