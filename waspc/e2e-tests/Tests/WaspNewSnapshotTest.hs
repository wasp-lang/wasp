module Tests.WaspNewSnapshotTest (waspNewSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.TestCommands (createSnapshotWaspProjectFromMinimalStarter)

waspNewSnapshotTest :: SnapshotTest
waspNewSnapshotTest =
  makeSnapshotTest
    "wasp-new"
    [createSnapshotWaspProjectFromMinimalStarter]
