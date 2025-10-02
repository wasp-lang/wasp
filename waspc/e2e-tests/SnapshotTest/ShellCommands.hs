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
  ( GitRootDir,
  )
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    waspCliNewMinimalStarter,
    (~&&),
    (~|),
  )
import SnapshotTest.FileSystem (SnapshotDir, gitRootFromSnapshotDir)
import StrongPath (Abs, Dir, Path', Rel, fromAbsDir, fromRelDir, (</>))
import WaspProject.ShellCommands (WaspProjectContext (..))

-- | Shell commands executed with this context are run from the 'SnapshotTest.FileSystem.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  { _snapshotDir :: Path' Abs (Dir SnapshotDir),
    _snapshotWaspProjectContext :: WaspProjectContext
  }

createSnapshotWaspProjectFromMinimalStarter :: ShellCommandBuilder SnapshotTestContext ShellCommand
createSnapshotWaspProjectFromMinimalStarter = do
  snapshotTestContext <- ask
  waspCliNewMinimalStarter $ _waspProjectName $ _snapshotWaspProjectContext snapshotTestContext

withInSnapshotWaspProjectDir ::
  [ShellCommandBuilder WaspProjectContext ShellCommand] ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
withInSnapshotWaspProjectDir waspProjectCommandBuilders = do
  snapshotTestContext <- ask
  return $
    "cd " ++ fromAbsDir (_waspProjectDir $ _snapshotWaspProjectContext snapshotTestContext)
      ~&& foldr1 (~&&) (snapshotWaspProjectCommands snapshotTestContext)
      ~&& "cd " ++ fromAbsDir (_snapshotDir snapshotTestContext)
  where
    snapshotWaspProjectCommands :: SnapshotTestContext -> [ShellCommand]
    snapshotWaspProjectCommands snapshotTestContext =
      buildShellCommand (_snapshotWaspProjectContext snapshotTestContext) $ sequence waspProjectCommandBuilders

copyContentsOfGitTrackedDirToSnapshotWaspProjectDir ::
  Path' (Rel GitRootDir) (Dir srcDir) ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
copyContentsOfGitTrackedDirToSnapshotWaspProjectDir srcDirFromGitRootDir = do
  snapshotTestContext <- ask
  let snapshotWaspProjectDir = _waspProjectDir $ _snapshotWaspProjectContext snapshotTestContext

      listRelPathsOfGitTrackedFilesInSrcDir :: ShellCommand =
        "git -C " ++ fromRelDir gitRootFromSnapshotDir ++ " ls-files " ++ fromRelDir srcDirFromGitRootDir
      -- Remove the relative prefix from each path so that files get copied into the destination dir directly.
      -- e.g. `../../../../examples/todoApp/file.txt` -> `file.txt`
      stripSrcDirRelPrefixFromPaths :: ShellCommand =
        "sed 's#^" ++ fromRelDir srcDirFromGitRootDir ++ "##'"
      copyFromSrcDirToSnapshotWaspProjectDir :: ShellCommand =
        "rsync -a --files-from=- " ++ fromRelDir (gitRootFromSnapshotDir </> srcDirFromGitRootDir) ++ " " ++ fromAbsDir snapshotWaspProjectDir
   in return $
        "mkdir -p " ++ fromAbsDir snapshotWaspProjectDir
          ~&& listRelPathsOfGitTrackedFilesInSrcDir
            ~| stripSrcDirRelPrefixFromPaths
            ~| copyFromSrcDirToSnapshotWaspProjectDir
