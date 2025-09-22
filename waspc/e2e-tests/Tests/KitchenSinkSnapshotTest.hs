module Tests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( copyContentsOfGitTrackedDirToSnapshotTestWaspAppDir,
    withInSnapshotTestWaspAppDir,
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
    [ copyContentsOfGitTrackedDirToSnapshotTestWaspAppDir [reldir|waspc/examples/todoApp/|],
      withInSnapshotTestWaspAppDir
        [ createDotEnvServerFile,
          waspCliCompile
        ]
    ]
  where
    createDotEnvServerFile :: ShellCommandBuilder WaspAppContext ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"
