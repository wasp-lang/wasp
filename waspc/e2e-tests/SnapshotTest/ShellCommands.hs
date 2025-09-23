{-# LANGUAGE TypeSynonymInstances #-}

module SnapshotTest.ShellCommands
  ( SnapshotTestContext (..),
    createSnapshotWaspApp,
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
snapshotTestContextToWaspAppContext ctx =
  WaspAppContext {_waspAppName = dropTrailingPathSeparator $ fromRelDir $ _snapshotWaspAppRelDir ctx}

createSnapshotWaspApp :: ShellCommandBuilder SnapshotTestContext ShellCommand
createSnapshotWaspApp = do
  snapshotTestContext <- ask
  waspCliNewMinimalStarter $ _waspAppName $ snapshotTestContextToWaspAppContext snapshotTestContext

withInSnapshotWaspAppDir ::
  [ShellCommandBuilder WaspAppContext ShellCommand] ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
withInSnapshotWaspAppDir waspAppCommandBuilders = do
  snapshotTestContext <- ask
  let waspAppContext = snapshotTestContextToWaspAppContext snapshotTestContext

      navigateToSnapshotWaspAppDir :: ShellCommand =
        "pushd " ++ fromAbsDir (_snapshotAbsDir snapshotTestContext </> _snapshotWaspAppRelDir snapshotTestContext)
      waspAppCommand :: ShellCommand =
        foldr1 ($&&) $ buildShellCommand waspAppContext $ sequence waspAppCommandBuilders
      returnToSnapshotDir :: ShellCommand =
        "popd"
   in return $ navigateToSnapshotWaspAppDir $&& waspAppCommand $&& returnToSnapshotDir

copyContentsOfGitTrackedDirToSnapshotWaspAppDir ::
  Path' (Rel GitRepositoryRoot) (Dir SnapshotWaspAppDir) ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
copyContentsOfGitTrackedDirToSnapshotWaspAppDir srcDirInGitRoot = do
  snapshotTestContext <- ask
  let srcDirPath = fromRelDir (gitRootInSnapshotWaspAppDir </> srcDirInGitRoot)
      destDirPath = fromRelDir $ _snapshotWaspAppRelDir snapshotTestContext

      createDestDir :: ShellCommand =
        "mkdir -p " ++ destDirPath
      listRelPathsOfGitTrackedFilesInSrcDir :: ShellCommand =
        "git -C " ++ fromRelDir gitRootInSnapshotWaspAppDir ++ " ls-files " ++ fromRelDir srcDirInGitRoot
      -- Remove the src dir prefix from each path so that files get copied into the destination dir directly.
      -- e.g. `waspc/examples/todoApp/file.txt` -> `file.txt`
      stripSrcDirPrefixFromPaths :: ShellCommand =
        "sed 's#^" ++ fromRelDir srcDirInGitRoot ++ "##'"
      copyFromSrcDirToDestDir :: ShellCommand =
        "rsync -a --files-from=- " ++ srcDirPath ++ " " ++ destDirPath
   in return $
        createDestDir
          $&& listRelPathsOfGitTrackedFilesInSrcDir
            $| stripSrcDirPrefixFromPaths
            $| copyFromSrcDirToDestDir
