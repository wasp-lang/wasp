module Tests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotWaspApp,
    withInSnapshotWaspAppDir,
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
    [ createSnapshotWaspApp,
      withInSnapshotWaspAppDir
        [ setWaspDbToPSQL,
          waspCliBuild,
          buildWaspDockerImage
        ]
    ]
