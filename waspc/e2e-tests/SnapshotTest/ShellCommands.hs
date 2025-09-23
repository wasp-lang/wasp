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
    ($&&),
    ($|),
  )
import SnapshotTest.FileSystem (SnapshotWaspAppDir, gitRootInSnapshotWaspAppDir, snapshotWaspAppDirInSnapshotDir)
import StrongPath (Dir, Path', Rel, fromRelDir)
import qualified StrongPath as SP
import WaspApp.ShellCommands (WaspAppContext (..))

-- | Shell commands executed with this context are run from the 'SnapshotTest.FileSystem.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  deriving (Show)

snapshotTestContextToWaspAppContext :: SnapshotTestContext -> WaspAppContext
snapshotTestContextToWaspAppContext _snapshotTestContext =
  WaspAppContext
    { _waspAppName = "wasp-app" -- NOTE: we hardcode the Wasp app name so the snapshots directory is more readable.
    }

createSnapshotWaspApp :: ShellCommandBuilder SnapshotTestContext ShellCommand
createSnapshotWaspApp = do
  snapshotTestContext <- ask
  let waspAppContext = snapshotTestContextToWaspAppContext snapshotTestContext

  waspCliNewMinimalStarter $ _waspAppName waspAppContext
  where
    waspCliNewMinimalStarter :: String -> ShellCommandBuilder ctx ShellCommand
    waspCliNewMinimalStarter appName = do
      return $
        "wasp-cli new " ++ appName ++ " -t minimal"

withInSnapshotWaspAppDir :: [ShellCommandBuilder WaspAppContext ShellCommand] -> ShellCommandBuilder SnapshotTestContext ShellCommand
withInSnapshotWaspAppDir waspAppCommandBuilders = do
  snapshotTestContext <- ask
  let waspAppContext = snapshotTestContextToWaspAppContext snapshotTestContext
  let snapshotWaspAppRelDir = snapshotWaspAppDirInSnapshotDir $ _waspAppName waspAppContext

  let navigateToSnapshotWaspAppDir = "pushd " ++ fromRelDir snapshotWaspAppRelDir
  let waspAppCommand = foldr1 ($&&) $ buildShellCommand waspAppContext $ sequence waspAppCommandBuilders
  let returnToSnapshotDir = "popd"

  return $ navigateToSnapshotWaspAppDir $&& waspAppCommand $&& returnToSnapshotDir

copyContentsOfGitTrackedDirToSnapshotWaspAppDir ::
  Path' (Rel GitRepositoryRoot) (Dir SnapshotWaspAppDir) ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
copyContentsOfGitTrackedDirToSnapshotWaspAppDir srcDirInGitRoot = do
  snapshotTestContext <- ask
  let srcDirPath = fromRelDir (gitRootInSnapshotWaspAppDir SP.</> srcDirInGitRoot)
      waspAppContext = snapshotTestContextToWaspAppContext snapshotTestContext
      destDirPath = "./" ++ _waspAppName waspAppContext

      createDestDir :: ShellCommand = "mkdir -p " ++ destDirPath

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
