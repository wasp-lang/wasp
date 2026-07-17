module Wasp.Cli.ProjectLockTest where

import Control.Exception (try)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.Exit (ExitCode (ExitFailure))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Wasp.Cli.Command (CommandError (CommandError), runCommand)
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Require (LockedWaspProject (LockedWaspProject), require)
import Wasp.Cli.ProjectLock
  ( ProjectLockError (..),
    acquireProjectLock,
    getCurrentWaspProcessId,
    projectLockFilePath,
    releaseProjectLock,
  )
import Wasp.Project.Common (WaspProjectDir)

spec_projectLock :: Spec
spec_projectLock = do
  describe "project lock file" $ do
    it "rejects a lock owned by a running process" $
      withTempWaspProject $ \waspProjectDir -> do
        processId <- getCurrentWaspProcessId
        writeLockFile waspProjectDir $ show processId

        acquireProjectLock waspProjectDir
          `shouldReturn` Left (ProjectLockHeld processId)

    it "reclaims a lock owned by a dead process" $
      withTempWaspProject $ \waspProjectDir -> do
        writeLockFile waspProjectDir "999999999"

        acquireProjectLock waspProjectDir >>= \case
          Left lockError -> expectationFailure $ "Expected to acquire lock, got: " ++ show lockError
          Right _ -> releaseProjectLock waspProjectDir

    it "fails safely when the lock file is malformed" $
      withTempWaspProject $ \waspProjectDir -> do
        writeLockFile waspProjectDir "not a process ID"

        acquireProjectLock waspProjectDir
          `shouldReturn` Left ProjectLockMalformed

    it "can be acquired again after release" $
      withTempWaspProject $ \waspProjectDir -> do
        _ <- expectAcquired =<< acquireProjectLock waspProjectDir
        releaseProjectLock waspProjectDir
        _ <- expectAcquired =<< acquireProjectLock waspProjectDir
        releaseProjectLock waspProjectDir

  describe "LockedWaspProject requirement" $ do
    -- These cases change the process-wide current directory, so they must remain
    -- in a single test to prevent tasty from running them concurrently.
    it "handles command cleanup and clean" $ do
      withTempWaspProject $ \waspProjectDir -> do
        Directory.withCurrentDirectory (SP.fromAbsDir waspProjectDir) $
          runCommand $ do
            LockedWaspProject lockedProjectDir <- require
            LockedWaspProject cachedProjectDir <- require
            liftIO $ do
              lockedProjectDir `shouldBe` waspProjectDir
              cachedProjectDir `shouldBe` waspProjectDir
              Directory.doesFileExist (lockFilePath waspProjectDir)
                `shouldReturn` True

        Directory.doesFileExist (lockFilePath waspProjectDir) `shouldReturn` False

      withTempWaspProject $ \waspProjectDir -> do
        result <-
          Directory.withCurrentDirectory (SP.fromAbsDir waspProjectDir) $
            try $
              runCommand $ do
                LockedWaspProject _ <- require
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

lockFilePath :: Path' Abs (Dir WaspProjectDir) -> FilePath
lockFilePath = SP.fromAbsFile . projectLockFilePath
