{-# LANGUAGE TypeSynonymInstances #-}

module GoldenTest.ShellCommands
  ( GoldenTestContext (..),
    createGoldenTestWaspApp,
    withInGoldenTestWaspAppDir,
    copyContentsOfGitTrackedDirToGoldenTestWaspAppDir,
  )
where

import Common
  ( GitRepositoryRoot,
  )
import Control.Monad.Reader (MonadReader (ask))
import GoldenTest.Snapshot (SnapshotWaspAppDir, gitRootInSnapshotWaspAppDir, snapshotWaspAppDirInSnapshotDir)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    combineShellCommands,
    ($|),
  )
import StrongPath (Dir, Path', Rel, fromRelDir)
import qualified StrongPath as SP
import WaspApp.ShellCommands (WaspAppContext (..))

-- | Shell commands executed with this context are run from the 'GoldenTest.Common.SnapshotDir' directory.
data GoldenTestContext = GoldenTestContext
  {_contextGoldenTestName :: String}
  deriving (Show)

goldenTestContextToWaspAppContext :: GoldenTestContext -> WaspAppContext
goldenTestContextToWaspAppContext _goldenTestContext =
  WaspAppContext
    { _waspAppName = "wasp-app" -- NOTE: we hardcode the Wasp app name so the snapshots folders is more readable.
    }

createGoldenTestWaspApp :: ShellCommandBuilder GoldenTestContext ShellCommand
createGoldenTestWaspApp = do
  goldenTestContext <- ask
  let waspAppContext = goldenTestContextToWaspAppContext goldenTestContext

  waspCliNewMinimalStarter $ _waspAppName waspAppContext
  where
    waspCliNewMinimalStarter :: String -> ShellCommandBuilder ctx ShellCommand
    waspCliNewMinimalStarter appName = do
      return $
        "wasp-cli new " ++ appName ++ " -t minimal"

withInGoldenTestWaspAppDir :: [ShellCommandBuilder WaspAppContext ShellCommand] -> ShellCommandBuilder GoldenTestContext ShellCommand
withInGoldenTestWaspAppDir waspAppCommandBuilders = do
  goldenTestContext <- ask
  let waspAppContext = goldenTestContextToWaspAppContext goldenTestContext
  let goldenTestWaspAppRelDir = snapshotWaspAppDirInSnapshotDir $ _waspAppName waspAppContext

  let navigateToGoldenTestWaspAppDir :: ShellCommand = "pushd " ++ fromRelDir goldenTestWaspAppRelDir
  let waspAppCommand :: ShellCommand = combineShellCommands $ buildShellCommand waspAppContext $ sequence waspAppCommandBuilders
  let returnToSnapshotDir :: ShellCommand = "popd"

  return $ combineShellCommands [navigateToGoldenTestWaspAppDir, waspAppCommand, returnToSnapshotDir]

copyContentsOfGitTrackedDirToGoldenTestWaspAppDir ::
  Path' (Rel GitRepositoryRoot) (Dir SnapshotWaspAppDir) ->
  ShellCommandBuilder GoldenTestContext ShellCommand
copyContentsOfGitTrackedDirToGoldenTestWaspAppDir srcDirInGitRoot = do
  goldenTestContext <- ask
  let srcDirPath = fromRelDir (gitRootInSnapshotWaspAppDir SP.</> srcDirInGitRoot)
      waspAppContext = goldenTestContextToWaspAppContext goldenTestContext
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
