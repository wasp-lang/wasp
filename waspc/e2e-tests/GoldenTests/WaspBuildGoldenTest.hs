module GoldenTests.WaspBuildGoldenTest (waspBuildGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( buildDockerImage,
    setWaspDbToPSQL,
    waspCliBuild,
    waspCliNewMinimalStarter,
    withInSnapshotProjectDir,
  )

waspBuildGoldenTest :: GoldenTest
waspBuildGoldenTest =
  makeGoldenTest "wasp-build" $
    sequence
      [ waspCliNewMinimalStarter "wasp-build",
        withInSnapshotProjectDir $
          sequence
            [ setWaspDbToPSQL,
              waspCliBuild,
              buildDockerImage
            ]
      ]
