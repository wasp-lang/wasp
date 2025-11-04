module Tests.WaspNewSnapshotTest (waspNewSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands (createSnapshotWaspProjectFromMinimalStarter)

waspNewSnapshotTest :: SnapshotTest
waspNewSnapshotTest =
  makeSnapshotTest
    "wasp-new"
    [createSnapshotWaspProjectFromMinimalStarter]
