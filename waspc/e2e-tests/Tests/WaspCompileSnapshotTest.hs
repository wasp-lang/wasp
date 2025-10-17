module Tests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotWaspProjectFromMinimalStarter,
    withInSnapshotWaspProjectDir,
  )
import WaspProject.ShellCommands
  ( waspCliCompile,
  )

waspCompileSnapshotTest :: SnapshotTest
waspCompileSnapshotTest =
  makeSnapshotTest
    "wasp-compile"
    [ createSnapshotWaspProjectFromMinimalStarter,
      withInSnapshotWaspProjectDir
        [waspCliCompile]
    ]
