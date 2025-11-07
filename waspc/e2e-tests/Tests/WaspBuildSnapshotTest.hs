module Tests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.TestCommands
  ( createSnapshotWaspProjectFromMinimalStarter,
    withInSnapshotWaspProjectDir,
  )
import WaspProject.TestCommands
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
