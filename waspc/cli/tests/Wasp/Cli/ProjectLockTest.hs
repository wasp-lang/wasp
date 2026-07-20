module Wasp.Cli.ProjectLockTest where

import Control.Exception (try)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, File, Path', (</>))
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.Exit (ExitCode (ExitFailure))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Wasp.Cli.Command (CommandError (CommandError), runCommand)
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Require (InLockedWaspProject (InLockedWaspProject), require)
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

  describe "InLockedWaspProject requirement" $ do
    -- These cases change the process-wide current directory, so they must remain
    -- in a single test to prevent tasty from running them concurrently.
    it "handles command cleanup and clean" $ do
      withTempWaspProject $ \waspProjectDir -> do
        Directory.withCurrentDirectory (SP.fromAbsDir waspProjectDir) $
          runCommand $ do
            InLockedWaspProject lockedProjectDir <- require
            liftIO $ do
              lockedProjectDir `shouldBe` waspProjectDir
              Directory.doesFileExist (lockFilePath waspProjectDir)
                `shouldReturn` True

        Directory.doesFileExist (lockFilePath waspProjectDir) `shouldReturn` False

      withTempWaspProject $ \waspProjectDir -> do
        result <-
          Directory.withCurrentDirectory (SP.fromAbsDir waspProjectDir) $
            try $
              runCommand $ do
                InLockedWaspProject _ <- require
                throwError $ CommandError "Test error" "Error message"

        result `shouldBe` Left (ExitFailure 1)
        Directory.doesFileExist (lockFilePath waspProjectDir) `shouldReturn` False

      withTempWaspProject $ \waspProjectDir -> do
        let dotWaspDir = SP.fromAbsDir waspProjectDir ++ ".wasp"
        let nodeModulesDir = SP.fromAbsDir waspProjectDir ++ "node_modules"
        Directory.createDirectoryIfMissing True dotWaspDir
        Directory.createDirectoryIfMissing True nodeModulesDir

        Directory.withCurrentDirectory (SP.fromAbsDir waspProjectDir) $ runCommand clean

        Directory.doesDirectoryExist dotWaspDir `shouldReturn` False
        Directory.doesDirectoryExist nodeModulesDir `shouldReturn` False

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
