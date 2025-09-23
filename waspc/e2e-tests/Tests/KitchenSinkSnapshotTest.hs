module Tests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( copyContentsOfGitTrackedDirToSnapshotWaspAppDir,
    withInSnapshotWaspAppDir,
  )
import StrongPath (reldir)
import WaspApp.ShellCommands
  ( WaspAppContext,
    waspCliCompile,
  )

kitchenSinkSnapshotTest :: SnapshotTest
kitchenSinkSnapshotTest =
  makeSnapshotTest
    "kitchen-sink"
    [ copyContentsOfGitTrackedDirToSnapshotWaspAppDir [reldir|waspc/examples/todoApp/|],
      withInSnapshotWaspAppDir
        [ createDotEnvServerFile,
          waspCliCompile
        ]
    ]
  where
    createDotEnvServerFile :: ShellCommandBuilder WaspAppContext ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"
