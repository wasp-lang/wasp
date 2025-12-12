module SnapshotTest.WaspBuildSnapshotTest (waspBuildSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.ShellCommands
  ( createSnapshotWaspProjectFromMinimalStarter,
    withInSnapshotWaspProjectDir,
  )
import WaspProject.ShellCommands
  ( buildAndRemoveWaspProjectDockerImage,
    setWaspDbToPSQL,
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
          buildAndRemoveWaspProjectDockerImage
        ]
    ]
