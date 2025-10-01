module Tests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
    withInSnapshotWaspProjectDir,
  )
import StrongPath (reldir)
import WaspProject.ShellCommands
  ( WaspProjectContext,
    waspCliCompile,
  )

kitchenSinkSnapshotTest :: SnapshotTest
kitchenSinkSnapshotTest =
  makeSnapshotTest
    "kitchen-sink"
    [ copyContentsOfGitTrackedDirToSnapshotWaspProjectDir [reldir|waspc/examples/todoApp/|],
      withInSnapshotWaspProjectDir
        [ createDotEnvServerFile,
          waspCliCompile
        ]
    ]
  where
    createDotEnvServerFile :: ShellCommandBuilder WaspProjectContext ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"
