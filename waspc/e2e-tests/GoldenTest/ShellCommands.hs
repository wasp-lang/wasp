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
import GoldenTest.Common
  ( GoldenTestWaspAppDir,
    gitRootInGoldenTestWaspAppDir,
    goldenTestWaspAppDirInGoldenTestSnapshotDir,
  )
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    combineShellCommands,
    waspCliNewMinimalStarter,
    ($|),
  )
import StrongPath (Dir, Path', Rel, fromRelDir)
import qualified StrongPath as SP
import WaspApp.ShellCommands (WaspAppContext (..))

-- | Context for commands which are run from the golden tests.
-- This context is executed from the "GoldenTest.Common.SnapshotDir" directory.
data GoldenTestContext = GoldenTestContext
  {_goldenTestProjectName :: String}
  deriving (Show)

goldenTestContextToWaspAppContext :: GoldenTestContext -> WaspAppContext
goldenTestContextToWaspAppContext ctx =
  WaspAppContext
    { _waspAppName = _goldenTestProjectName ctx
    }

createGoldenTestWaspApp :: ShellCommandBuilder GoldenTestContext ShellCommand
createGoldenTestWaspApp = do
  goldenTestContext <- ask
  let waspAppContext = goldenTestContextToWaspAppContext goldenTestContext

  waspCliNewMinimalStarter $ _waspAppName waspAppContext

withInGoldenTestWaspAppDir :: [ShellCommandBuilder WaspAppContext ShellCommand] -> ShellCommandBuilder GoldenTestContext ShellCommand
withInGoldenTestWaspAppDir waspAppCommandBuilders = do
  goldenTestContext <- ask
  let waspAppContext = goldenTestContextToWaspAppContext goldenTestContext
  let goldenTestWaspAppRelDir = goldenTestWaspAppDirInGoldenTestSnapshotDir $ _waspAppName waspAppContext

  let navigateToGoldenTestWaspAppDir :: ShellCommand = "pushd " ++ fromRelDir goldenTestWaspAppRelDir
  let waspAppCommand :: ShellCommand = combineShellCommands $ buildShellCommand waspAppContext $ sequence waspAppCommandBuilders
  let returnToSnapshotDir :: ShellCommand = "popd"

  return $ combineShellCommands [navigateToGoldenTestWaspAppDir, waspAppCommand, returnToSnapshotDir]

copyContentsOfGitTrackedDirToGoldenTestWaspAppDir ::
  Path' (Rel GitRepositoryRoot) (Dir GoldenTestWaspAppDir) ->
  ShellCommandBuilder GoldenTestContext ShellCommand
copyContentsOfGitTrackedDirToGoldenTestWaspAppDir srcDirInGitRoot = do
  goldenTestContext <- ask
  let srcDirPath = fromRelDir (gitRootInGoldenTestWaspAppDir SP.</> srcDirInGitRoot)
      destDirPath = "./" ++ _goldenTestProjectName goldenTestContext

      createDestDir :: ShellCommand = "mkdir -p " ++ destDirPath

      listRelPathsOfGitTrackedFilesInSrcDir :: ShellCommand =
        "git -C " ++ fromRelDir gitRootInGoldenTestWaspAppDir ++ " ls-files " ++ fromRelDir srcDirInGitRoot
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
