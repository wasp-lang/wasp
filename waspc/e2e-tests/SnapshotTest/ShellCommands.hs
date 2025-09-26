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
    (~&&),
    (~|),
  )
import SnapshotTest.FileSystem (SnapshotDir, SnapshotWaspAppDir, gitRootInSnapshotWaspAppDir)
import StrongPath (Abs, Dir, Path', Rel, fromAbsDir, fromRelDir, (</>))
import WaspApp.ShellCommands (WaspAppContext (..))

-- | Shell commands executed with this context are run from the 'SnapshotTest.FileSystem.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  { _snapshotDir :: Path' Abs (Dir SnapshotDir),
    _snapshotWaspAppDirInSnapshotDir :: Path' (Rel SnapshotDir) (Dir SnapshotWaspAppDir),
    _snapshotWaspAppName :: String
  }

getSnapshotWaspAppContext :: SnapshotTestContext -> WaspAppContext
getSnapshotWaspAppContext snapshotTestContext =
  WaspAppContext
    { _waspAppName = _snapshotWaspAppName snapshotTestContext,
      _waspAppDir = _snapshotDir snapshotTestContext </> _snapshotWaspAppDirInSnapshotDir snapshotTestContext
    }

createSnapshotWaspAppFromMinimalStarter :: ShellCommandBuilder SnapshotTestContext ShellCommand
createSnapshotWaspAppFromMinimalStarter = do
  snapshotTestContext <- ask
  waspCliNewMinimalStarter $ _snapshotWaspAppName snapshotTestContext

withInSnapshotWaspAppDir ::
  [ShellCommandBuilder WaspAppContext ShellCommand] ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
withInSnapshotWaspAppDir waspAppCommandBuilders = do
  snapshotTestContext <- ask
  let waspAppContext = getSnapshotWaspAppContext snapshotTestContext

  let snapshotAbsDir = _snapshotDir snapshotTestContext
  let snapshotWaspAppAbsDir = snapshotAbsDir </> _snapshotWaspAppDirInSnapshotDir snapshotTestContext

  let navigateToSnapshotWaspAppDir = "cd " ++ fromAbsDir snapshotWaspAppAbsDir
  let cmdInWaspAppContext = foldr1 (~&&) $ buildShellCommand waspAppContext $ sequence waspAppCommandBuilders
  let returnToSnapshotDir = "cd " ++ fromAbsDir snapshotAbsDir

  return $
    navigateToSnapshotWaspAppDir
      ~&& cmdInWaspAppContext
      ~&& returnToSnapshotDir

copyContentsOfGitTrackedDirToSnapshotWaspAppDir ::
  Path' (Rel GitRepositoryRoot) (Dir SnapshotWaspAppDir) ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
copyContentsOfGitTrackedDirToSnapshotWaspAppDir srcDirInRepoRoot = do
  snapshotTestContext <- ask
  let srcDirPath = fromRelDir (gitRootInSnapshotWaspAppDir </> srcDirInRepoRoot)
      destDirPath = fromRelDir $ _snapshotWaspAppDirInSnapshotDir snapshotTestContext

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
          ~&& listRelPathsOfGitTrackedFilesInSrcDir
            ~| stripSrcDirPrefixFromPaths
            ~| copyFromSrcDirToDestDir
