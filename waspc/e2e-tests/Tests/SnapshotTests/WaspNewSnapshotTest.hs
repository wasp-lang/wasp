module Tests.SnapshotTests.WaspNewSnapshotTest (waspNewSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Steps (createSnapshotWaspProjectFromMinimalStarter)

waspNewSnapshotTest :: SnapshotTest
waspNewSnapshotTest =
  makeSnapshotTest "wasp-new" createSnapshotWaspProjectFromMinimalStarter
