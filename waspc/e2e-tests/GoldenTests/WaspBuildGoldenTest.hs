module GoldenTests.WaspBuildGoldenTest (waspBuildGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( buildDockerImage,
    cdIntoGoldenTestProject,
    setWaspDbToPSQL,
    waspCliBuild,
    waspCliNewMinimalStarter,
  )

waspBuildGoldenTest :: GoldenTest
waspBuildGoldenTest =
  makeGoldenTest "wasp-build" $
    sequence
      [ waspCliNewMinimalStarter "wasp-build",
        cdIntoGoldenTestProject $
          sequence
            [ setWaspDbToPSQL,
              waspCliBuild,
              buildDockerImage
            ]
      ]
