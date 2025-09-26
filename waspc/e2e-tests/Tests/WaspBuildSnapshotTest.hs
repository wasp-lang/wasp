module Tests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotWaspAppFromMinimalStarter,
    withInSnapshotWaspAppDir,
  )
import WaspApp.ShellCommands
  ( setWaspDbToPSQL,
    validateWaspAppDockerImageBuilds,
    waspCliBuild,
  )

waspBuildSnapshotTest :: SnapshotTest
waspBuildSnapshotTest =
  makeSnapshotTest
    "wasp-build"
    [ createSnapshotWaspAppFromMinimalStarter,
      withInSnapshotWaspAppDir
        [ setWaspDbToPSQL,
          waspCliBuild,
          validateWaspAppDockerImageBuilds
        ]
    ]
