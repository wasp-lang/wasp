module Tests.WaspNewSnapshotTest (waspNewSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands (createSnapshotWaspAppFromMinimalStarter)

waspNewSnapshotTest :: SnapshotTest
waspNewSnapshotTest =
  makeSnapshotTest
    "wasp-new"
    [createSnapshotWaspAppFromMinimalStarter]
