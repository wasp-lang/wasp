{-# LANGUAGE TypeSynonymInstances #-}

module SnapshotTest.TestCommands
  ( SnapshotTestContext (..),
    createSnapshotWaspProjectFromMinimalStarter,
    withInSnapshotWaspProjectDir,
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import FileSystem
  ( GitRootDir,
  )
import SnapshotTest.FileSystem (SnapshotDir, gitRootFromSnapshotDir)
import StrongPath (Abs, Dir, Path', Rel, fromAbsDir, fromRelDir)
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    withCurrentDirectory,
  )
import System.FilePath (takeDirectory)
import qualified System.FilePath as FP
import System.Process (readProcess)
import TestCommands
  ( TestCommand (..),
    runTestCommand,
    waspCliNewMinimalStarter,
  )
import WaspProject.TestCommands (WaspProjectContext (..))

-- | Test commands executed with this context are run from the 'SnapshotTest.FileSystem.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  { _snapshotDir :: Path' Abs (Dir SnapshotDir),
    _snapshotWaspProjectContext :: WaspProjectContext
  }

createSnapshotWaspProjectFromMinimalStarter :: TestCommand SnapshotTestContext ()
createSnapshotWaspProjectFromMinimalStarter = do
  snapshotTestContext <- ask
  waspCliNewMinimalStarter $ _waspProjectName $ _snapshotWaspProjectContext snapshotTestContext

withInSnapshotWaspProjectDir ::
  [TestCommand WaspProjectContext ()] ->
  TestCommand SnapshotTestContext ()
withInSnapshotWaspProjectDir waspProjectCommands = do
  snapshotTestContext <- ask
  let projectDir = _waspProjectDir $ _snapshotWaspProjectContext snapshotTestContext
  let waspCtx = _snapshotWaspProjectContext snapshotTestContext

  liftIO $ withCurrentDirectory (fromAbsDir projectDir) $
    mapM_ (runTestCommand waspCtx) waspProjectCommands

copyContentsOfGitTrackedDirToSnapshotWaspProjectDir ::
  Path' (Rel GitRootDir) (Dir srcDir) ->
  TestCommand SnapshotTestContext ()
copyContentsOfGitTrackedDirToSnapshotWaspProjectDir srcDirFromGitRootDir = do
  snapshotTestContext <- ask
  let snapshotWaspProjectDir = _waspProjectDir $ _snapshotWaspProjectContext snapshotTestContext
  let srcDirPath = fromRelDir srcDirFromGitRootDir

  liftIO $ do
    -- Ensure the wasp project directory exists
    createDirectoryIfMissing True (fromAbsDir snapshotWaspProjectDir)

    -- Get list of git-tracked files in the source directory
    let gitRootPath = fromRelDir gitRootFromSnapshotDir
    gitOutput <-
      readProcess
        "git"
        ["-C", gitRootPath, "ls-files", srcDirPath]
        ""

    let gitTrackedFiles = lines gitOutput

    -- Copy each file to the destination
    forM_ gitTrackedFiles $ \fullRelPath -> do
      -- Remove the source directory prefix to get the relative path within srcDir
      let relPath = FP.makeRelative srcDirPath fullRelPath
      let srcFilePath = gitRootPath FP.</> fullRelPath
      let dstFilePath = fromAbsDir snapshotWaspProjectDir FP.</> relPath

      -- Create parent directories if needed
      createDirectoryIfMissing True (takeDirectory dstFilePath)

      -- Copy the file
      copyFile srcFilePath dstFilePath
