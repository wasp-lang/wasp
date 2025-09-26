module Tests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotWaspAppFromMinimalStarter,
    withInSnapshotWaspAppDir,
  )
import WaspApp.ShellCommands
  ( waspCliCompile,
  )

waspCompileSnapshotTest :: SnapshotTest
waspCompileSnapshotTest =
  makeSnapshotTest
    "wasp-compile"
    [ createSnapshotWaspAppFromMinimalStarter,
      withInSnapshotWaspAppDir
        [waspCliCompile]
    ]
