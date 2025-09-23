module Tests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotWaspApp,
    withInSnapshotWaspAppDir,
  )
import WaspApp.ShellCommands
  ( waspCliCompile,
  )

waspCompileSnapshotTest :: SnapshotTest
waspCompileSnapshotTest =
  makeSnapshotTest
    "wasp-compile"
    [ createSnapshotWaspApp,
      withInSnapshotWaspAppDir
        [waspCliCompile]
    ]
