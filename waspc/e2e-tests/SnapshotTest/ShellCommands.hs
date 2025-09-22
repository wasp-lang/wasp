{-# LANGUAGE TypeSynonymInstances #-}

module SnapshotTest.ShellCommands
  ( SnapshotTestContext (..),
    createSnapshotTestWaspApp,
    withInSnapshotTestWaspAppDir,
    copyContentsOfGitTrackedDirToSnapshotTestWaspAppDir,
  )
where

import Common
  ( GitRepositoryRoot,
  )
import Control.Monad.Reader (MonadReader (ask))
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    combineShellCommands,
    ($|),
  )
import SnapshotTest.Snapshot (SnapshotWaspAppDir, gitRootInSnapshotWaspAppDir, snapshotWaspAppDirInSnapshotDir)
import StrongPath (Dir, Path', Rel, fromRelDir)
import qualified StrongPath as SP
import WaspApp.ShellCommands (WaspAppContext (..))

-- | Shell commands executed with this context are run from the 'SnapshotTest.Common.SnapshotDir' directory.
data SnapshotTestContext = SnapshotTestContext
  deriving (Show)

snapshotTestContextToWaspAppContext :: SnapshotTestContext -> WaspAppContext
snapshotTestContextToWaspAppContext _snapshotTestContext =
  WaspAppContext
    { _waspAppName = "wasp-app" -- NOTE: we hardcode the Wasp app name so the snapshots directory is more readable.
    }

createSnapshotTestWaspApp :: ShellCommandBuilder SnapshotTestContext ShellCommand
createSnapshotTestWaspApp = do
  snapshotTestContext <- ask
  let waspAppContext = snapshotTestContextToWaspAppContext snapshotTestContext

  waspCliNewMinimalStarter $ _waspAppName waspAppContext
  where
    waspCliNewMinimalStarter :: String -> ShellCommandBuilder ctx ShellCommand
    waspCliNewMinimalStarter appName = do
      return $
        "wasp-cli new " ++ appName ++ " -t minimal"

withInSnapshotTestWaspAppDir :: [ShellCommandBuilder WaspAppContext ShellCommand] -> ShellCommandBuilder SnapshotTestContext ShellCommand
withInSnapshotTestWaspAppDir waspAppCommandBuilders = do
  snapshotTestContext <- ask
  let waspAppContext = snapshotTestContextToWaspAppContext snapshotTestContext
  let snapshotTestWaspAppRelDir = snapshotWaspAppDirInSnapshotDir $ _waspAppName waspAppContext

  let navigateToSnapshotTestWaspAppDir :: ShellCommand = "pushd " ++ fromRelDir snapshotTestWaspAppRelDir
  let waspAppCommand :: ShellCommand = combineShellCommands $ buildShellCommand waspAppContext $ sequence waspAppCommandBuilders
  let returnToSnapshotDir :: ShellCommand = "popd"

  return $ combineShellCommands [navigateToSnapshotTestWaspAppDir, waspAppCommand, returnToSnapshotDir]

copyContentsOfGitTrackedDirToSnapshotTestWaspAppDir ::
  Path' (Rel GitRepositoryRoot) (Dir SnapshotWaspAppDir) ->
  ShellCommandBuilder SnapshotTestContext ShellCommand
copyContentsOfGitTrackedDirToSnapshotTestWaspAppDir srcDirInGitRoot = do
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
        combineShellCommands
          [ createDestDir,
            listRelPathsOfGitTrackedFilesInSrcDir
              $| stripSrcDirPrefixFromPaths
              $| copyFromSrcDirToDestDir
          ]
