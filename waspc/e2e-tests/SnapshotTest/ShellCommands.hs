{-# LANGUAGE TypeSynonymInstances #-}

module SnapshotTest.ShellCommands
  ( SnapshotTestContext,
    defaultSnapshotTestContext,
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
import SnapshotTest.FileSystem (SnapshotWaspAppDir, gitRootInSnapshotWaspAppDir, snapshotWaspAppDirInSnapshotDir)
import StrongPath (Dir, Path', Rel, fromRelDir)
import qualified StrongPath as SP
import WaspApp.ShellCommands (WaspAppContext (..))

-- | Shell commands executed with this context are run from the 'SnapshotTest.FileSystem.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  {_defaultWaspAppName :: String}
  deriving (Show)

defaultSnapshotTestContext :: SnapshotTestContext
defaultSnapshotTestContext = SnapshotTestContext {_defaultWaspAppName = "wasp-app"}

snapshotTestContextToWaspAppContext :: SnapshotTestContext -> WaspAppContext
snapshotTestContextToWaspAppContext ctx = WaspAppContext {_waspAppName = _defaultWaspAppName ctx}

createSnapshotWaspApp :: ShellCommandBuilder SnapshotTestContext ShellCommand
createSnapshotWaspApp = do
  snapshotTestContext <- ask
  waspCliNewMinimalStarter $ _defaultWaspAppName snapshotTestContext

withInSnapshotWaspAppDir ::
  [ShellCommandBuilder WaspAppContext ShellCommand] ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
withInSnapshotWaspAppDir waspAppCommandBuilders = do
  snapshotTestContext <- ask
  let snapshotWaspAppRelDir = snapshotWaspAppDirInSnapshotDir $ _defaultWaspAppName snapshotTestContext
      waspAppContext = snapshotTestContextToWaspAppContext snapshotTestContext

      navigateToSnapshotWaspAppDir :: ShellCommand =
        "pushd " ++ fromRelDir snapshotWaspAppRelDir
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
  let srcDirPath = fromRelDir (gitRootInSnapshotWaspAppDir SP.</> srcDirInGitRoot)
      destDirPath = fromRelDir $ snapshotWaspAppDirInSnapshotDir $ _defaultWaspAppName snapshotTestContext

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
