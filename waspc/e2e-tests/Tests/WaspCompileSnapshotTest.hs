module Tests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotTestWaspApp,
    withInSnapshotTestWaspAppDir,
  )
import WaspApp.ShellCommands
  ( waspCliCompile,
  )

waspCompileSnapshotTest :: SnapshotTest
waspCompileSnapshotTest =
  makeSnapshotTest
    "wasp-compile"
    [ createSnapshotTestWaspApp,
      withInSnapshotTestWaspAppDir
        [waspCliCompile]
    ]
