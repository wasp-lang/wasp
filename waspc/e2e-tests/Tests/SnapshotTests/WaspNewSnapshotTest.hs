module Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest) where

import ShellCommands (createSnapshotWaspProjectFromMinimalStarter)
import SnapshotTest (SnapshotTest, makeSnapshotTest)

waspNewSnapshotTest :: SnapshotTest
waspNewSnapshotTest =
  makeSnapshotTest
    "wasp-new"
    [createSnapshotWaspProjectFromMinimalStarter]
