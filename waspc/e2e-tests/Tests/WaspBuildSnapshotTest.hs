module Tests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotTestWaspApp,
    withInSnapshotTestWaspAppDir,
  )
import WaspApp.ShellCommands
  ( buildWaspDockerImage,
    setWaspDbToPSQL,
    waspCliBuild,
  )

waspBuildSnapshotTest :: SnapshotTest
waspBuildSnapshotTest =
  makeSnapshotTest
    "wasp-build"
    [ createSnapshotTestWaspApp,
      withInSnapshotTestWaspAppDir
        [ setWaspDbToPSQL,
          waspCliBuild,
          buildWaspDockerImage
        ]
    ]
