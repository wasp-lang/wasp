module Tests.SnapshotTests.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import ShellCommands
  ( buildAndRemoveWaspProjectDockerImage,
    createSnapshotWaspProjectFromMinimalStarter,
    inSnapshotWaspProjectDir,
    setWaspDbToPSQL,
    waspCliBuild,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)

waspBuildSnapshotTest :: SnapshotTest
waspBuildSnapshotTest =
  makeSnapshotTest
    "wasp-build"
    [ createSnapshotWaspProjectFromMinimalStarter,
      inSnapshotWaspProjectDir
        [ setWaspDbToPSQL,
          waspCliBuild,
          buildAndRemoveWaspProjectDockerImage
        ]
    ]
