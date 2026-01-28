module Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import ShellCommands
  ( createSnapshotWaspProjectFromMinimalStarter,
    waspCliCompile,
    withInSnapshotWaspProjectDir,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)

waspCompileSnapshotTest :: SnapshotTest
waspCompileSnapshotTest =
  makeSnapshotTest
    "wasp-compile"
    [ createSnapshotWaspProjectFromMinimalStarter,
      withInSnapshotWaspProjectDir
        [waspCliCompile]
    ]
