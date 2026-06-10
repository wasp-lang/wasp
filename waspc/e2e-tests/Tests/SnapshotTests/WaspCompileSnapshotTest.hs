module Tests.SnapshotTests.WaspCompileSnapshotTest (waspCompileSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Steps
  ( createSnapshotWaspProjectFromMinimalStarter,
    inSnapshotWaspProjectDir,
    runCommand,
    waspCliCompile,
  )

waspCompileSnapshotTest :: SnapshotTest
waspCompileSnapshotTest =
  makeSnapshotTest
    "wasp-compile"
    [ createSnapshotWaspProjectFromMinimalStarter,
      inSnapshotWaspProjectDir
        [runCommand waspCliCompile]
    ]
