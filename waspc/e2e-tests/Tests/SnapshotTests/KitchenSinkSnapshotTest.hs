module Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
    inSnapshotWaspProjectDir,
    waspCliCompile,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import StrongPath (reldir)

kitchenSinkSnapshotTest :: SnapshotTest
kitchenSinkSnapshotTest =
  makeSnapshotTest
    "kitchen-sink"
    [ copyContentsOfGitTrackedDirToSnapshotWaspProjectDir [reldir|examples/kitchen-sink|],
      inSnapshotWaspProjectDir
        [ createDotEnvServerFile,
          normalizePostgresConnectionString,
          waspCliCompile
        ]
    ]
  where
    createDotEnvServerFile :: ShellCommandBuilder WaspProjectContext ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"

    normalizePostgresConnectionString :: ShellCommandBuilder WaspProjectContext ShellCommand
    normalizePostgresConnectionString = return "printf '\\nDATABASE_URL=mock-database-url\\n' >> .env.server"
