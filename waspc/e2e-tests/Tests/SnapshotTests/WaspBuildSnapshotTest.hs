module Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import ShellCommands
  ( buildAndRemoveWaspProjectDockerImage,
    createSnapshotWaspProjectFromMinimalStarter,
    setWaspDbToPSQL,
    waspCliBuild,
    withInSnapshotWaspProjectDir,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)

waspBuildSnapshotTest :: SnapshotTest
waspBuildSnapshotTest =
  makeSnapshotTest
    "wasp-build"
    [ createSnapshotWaspProjectFromMinimalStarter,
      withInSnapshotWaspProjectDir
        [ setWaspDbToPSQL,
          waspCliBuild,
          buildAndRemoveWaspProjectDockerImage
        ]
    ]
