{-# LANGUAGE TypeSynonymInstances #-}

module SnapshotTest.ShellCommands
  ( SnapshotTestContext (..),
    createSnapshotWaspAppFromMinimalStarter,
    withInSnapshotWaspAppDir,
    copyContentsOfGitTrackedDirToSnapshotWaspAppDir,
  )
where

import Control.Monad.Reader (MonadReader (ask))
import FileSystem
  ( GitRepositoryRoot,
  )
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    waspCliNewMinimalStarter,
    ($&&),
    ($|),
  )
import SnapshotTest.FileSystem (SnapshotDir, SnapshotWaspAppDir, gitRootInSnapshotWaspAppDir)
import StrongPath (Abs, Dir, Path', Rel, fromAbsDir, fromRelDir, (</>))
import System.FilePath (dropTrailingPathSeparator)
import WaspApp.ShellCommands (WaspAppContext (..))

-- | Shell commands executed with this context are run from the 'SnapshotTest.FileSystem.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  { _snapshotAbsDir :: Path' Abs (Dir SnapshotDir),
    _snapshotWaspAppRelDir :: Path' (Rel SnapshotDir) (Dir SnapshotWaspAppDir)
  }

snapshotTestContextToWaspAppContext :: SnapshotTestContext -> WaspAppContext
snapshotTestContextToWaspAppContext snapshotTestContext =
  WaspAppContext
    { _waspAppName = dropTrailingPathSeparator $ fromRelDir $ _snapshotWaspAppRelDir snapshotTestContext,
      _waspAppAbsDir = _snapshotAbsDir snapshotTestContext </> _snapshotWaspAppRelDir snapshotTestContext
    }

createSnapshotWaspAppFromMinimalStarter :: ShellCommandBuilder SnapshotTestContext ShellCommand
createSnapshotWaspAppFromMinimalStarter = do
  snapshotTestContext <- ask
  waspCliNewMinimalStarter $ _waspAppName $ snapshotTestContextToWaspAppContext snapshotTestContext

withInSnapshotWaspAppDir ::
  [ShellCommandBuilder WaspAppContext ShellCommand] ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
withInSnapshotWaspAppDir waspAppCommandBuilders = do
  snapshotTestContext <- ask
  let waspAppContext = snapshotTestContextToWaspAppContext snapshotTestContext

  let snapshotAbsDir = _snapshotAbsDir snapshotTestContext
  let snapshotWaspAppAbsDir = snapshotAbsDir </> _snapshotWaspAppRelDir snapshotTestContext

  let navigateToSnapshotWaspAppDir = "cd " ++ fromAbsDir snapshotWaspAppAbsDir
  let cmdInWaspAppContext = foldr1 ($&&) $ buildShellCommand waspAppContext $ sequence waspAppCommandBuilders
  let returnToSnapshotDir = "cd " ++ fromAbsDir snapshotAbsDir

  return $
    navigateToSnapshotWaspAppDir
      $&& cmdInWaspAppContext
      $&& returnToSnapshotDir

copyContentsOfGitTrackedDirToSnapshotWaspAppDir ::
  Path' (Rel GitRepositoryRoot) (Dir SnapshotWaspAppDir) ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
copyContentsOfGitTrackedDirToSnapshotWaspAppDir srcDirInRepoRoot = do
  snapshotTestContext <- ask
  let srcDirPath = fromRelDir (gitRootInSnapshotWaspAppDir </> srcDirInRepoRoot)
      destDirPath = fromRelDir $ _snapshotWaspAppRelDir snapshotTestContext

      createDestDir :: ShellCommand =
        "mkdir -p " ++ destDirPath
      listRelPathsOfGitTrackedFilesInSrcDir :: ShellCommand =
        "git -C " ++ fromRelDir gitRootInSnapshotWaspAppDir ++ " ls-files " ++ fromRelDir srcDirInRepoRoot
      -- Remove the src dir prefix from each path so that files get copied into the destination dir directly.
      -- e.g. `waspc/examples/todoApp/file.txt` -> `file.txt`
      stripSrcDirPrefixFromPaths :: ShellCommand =
        "sed 's#^" ++ fromRelDir srcDirInRepoRoot ++ "##'"
      copyFromSrcDirToDestDir :: ShellCommand =
        "rsync -a --files-from=- " ++ srcDirPath ++ " " ++ destDirPath
   in return $
        createDestDir
          $&& listRelPathsOfGitTrackedFilesInSrcDir
            $| stripSrcDirPrefixFromPaths
            $| copyFromSrcDirToDestDir
