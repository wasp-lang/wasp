module Tests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotWaspProjectFromMinimalStarter,
    withInSnapshotWaspProjectDir,
  )
import WaspProject.ShellCommands
  ( setWaspDbToPSQL,
    validateWaspProjectDockerImageBuilds,
    waspCliBuild,
  )

waspBuildSnapshotTest :: SnapshotTest
waspBuildSnapshotTest =
  makeSnapshotTest
    "wasp-build"
    [ createSnapshotWaspProjectFromMinimalStarter,
      withInSnapshotWaspProjectDir
        [ setWaspDbToPSQL,
          waspCliBuild,
          validateWaspProjectDockerImageBuilds
        ]
    ]
