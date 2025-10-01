{-# LANGUAGE TypeSynonymInstances #-}

module SnapshotTest.ShellCommands
  ( SnapshotTestContext (..),
    createSnapshotWaspProjectFromMinimalStarter,
    withInSnapshotWaspProjectDir,
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
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
import SnapshotTest.FileSystem (SnapshotDir, SnapshotWaspProjectDir, asWaspProjectDir, gitRootInSnapshotWaspProjectDir)
import StrongPath (Abs, Dir, Path', Rel, fromAbsDir, fromRelDir, (</>))
import WaspProject.ShellCommands (WaspProjectContext (..))

-- | Shell commands executed with this context are run from the 'SnapshotTest.FileSystem.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  { _snapshotDir :: Path' Abs (Dir SnapshotDir),
    _snapshotWaspProjectDirInSnapshotDir :: Path' (Rel SnapshotDir) (Dir SnapshotWaspProjectDir),
    _snapshotWaspProjectName :: String
  }

getSnapshotWaspProjectContext :: SnapshotTestContext -> WaspProjectContext
getSnapshotWaspProjectContext snapshotTestContext =
  WaspProjectContext
    { _waspProjectName = _snapshotWaspProjectName snapshotTestContext,
      _waspProjectDir = asWaspProjectDir (_snapshotDir snapshotTestContext </> _snapshotWaspProjectDirInSnapshotDir snapshotTestContext)
    }

createSnapshotWaspProjectFromMinimalStarter :: ShellCommandBuilder SnapshotTestContext ShellCommand
createSnapshotWaspProjectFromMinimalStarter = do
  snapshotTestContext <- ask
  waspCliNewMinimalStarter $ _snapshotWaspProjectName snapshotTestContext

withInSnapshotWaspProjectDir ::
  [ShellCommandBuilder WaspProjectContext ShellCommand] ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
withInSnapshotWaspProjectDir waspProjectCommandBuilders = do
  snapshotTestContext <- ask
  let waspProjectContext = getSnapshotWaspProjectContext snapshotTestContext

  let snapshotDir = _snapshotDir snapshotTestContext
  let snapshotWaspProjectDir = snapshotDir </> _snapshotWaspProjectDirInSnapshotDir snapshotTestContext

  let navigateToSnapshotWaspProjectDir = "cd " ++ fromAbsDir snapshotWaspProjectDir
  let cmdInWaspProjectContext = foldr1 (~&&) $ buildShellCommand waspProjectContext $ sequence waspProjectCommandBuilders
  let returnToSnapshotDir = "cd " ++ fromAbsDir snapshotDir

  return $
    navigateToSnapshotWaspProjectDir
      ~&& cmdInWaspProjectContext
      ~&& returnToSnapshotDir

copyContentsOfGitTrackedDirToSnapshotWaspProjectDir ::
  Path' (Rel GitRepositoryRoot) (Dir SnapshotWaspProjectDir) ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
copyContentsOfGitTrackedDirToSnapshotWaspProjectDir srcDirInRepoRoot = do
  snapshotTestContext <- ask
  let srcDirPath = fromRelDir (gitRootInSnapshotWaspProjectDir </> srcDirInRepoRoot)
      destDirPath = fromRelDir $ _snapshotWaspProjectDirInSnapshotDir snapshotTestContext

      createDestDir :: ShellCommand =
        "mkdir -p " ++ destDirPath
      listRelPathsOfGitTrackedFilesInSrcDir :: ShellCommand =
        "git -C " ++ fromRelDir gitRootInSnapshotWaspProjectDir ++ " ls-files " ++ fromRelDir srcDirInRepoRoot
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
