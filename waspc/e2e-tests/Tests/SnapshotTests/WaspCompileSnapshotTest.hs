module Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import ShellCommands
  ( createSnapshotWaspProjectFromMinimalStarter,
    inSnapshotWaspProjectDir,
    waspCliCompile,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)

waspCompileSnapshotTest :: SnapshotTest
waspCompileSnapshotTest =
  makeSnapshotTest
    "wasp-compile"
    [ createSnapshotWaspProjectFromMinimalStarter,
      inSnapshotWaspProjectDir
        [waspCliCompile]
    ]
