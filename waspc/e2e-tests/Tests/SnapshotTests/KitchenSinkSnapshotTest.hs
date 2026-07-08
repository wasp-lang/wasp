module Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    SnapshotTestContext,
    WaspProjectContext,
    copyContentsOfGitTrackedDirToSnapshotSubDir,
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
    inSnapshotWaspProjectDir,
    waspCliCompile,
    waspCliInstall,
    (~&&),
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import StrongPath (reldir)

kitchenSinkSnapshotTest :: SnapshotTest
kitchenSinkSnapshotTest =
  makeSnapshotTest
    "kitchen-sink"
    [ copyContentsOfGitTrackedDirToSnapshotWaspProjectDir [reldir|examples/kitchen-sink|],
      copyContentsOfGitTrackedDirToSnapshotSubDir [reldir|examples/module|] [reldir|module|],
      buildModuleDependency,
      inSnapshotWaspProjectDir
        [ createDotEnvServerFile,
          normalizePostgresConnectionString,
          waspCliInstall,
          waspCliCompile
        ],
      removeModuleDependency
    ]
  where
    buildModuleDependency :: ShellCommandBuilder SnapshotTestContext ShellCommand
    buildModuleDependency =
      return $
        "cd module"
          ~&& "$WASP_CLI_CMD module install"
          ~&& "$WASP_CLI_CMD module build"
          ~&& "npm run pack"
          ~&& "cd .."

    removeModuleDependency :: ShellCommandBuilder SnapshotTestContext ShellCommand
    removeModuleDependency = return "rm -rf module"

    createDotEnvServerFile :: ShellCommandBuilder WaspProjectContext ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"

    normalizePostgresConnectionString :: ShellCommandBuilder WaspProjectContext ShellCommand
    normalizePostgresConnectionString = return "printf '\\nDATABASE_URL=mock-database-url\\n' >> .env.server"
