module GoldenTests.WaspBuildGoldenTest (waspBuildGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( buildDockerImage,
    cdIntoCurrentProject,
    setWaspDbToPSQL,
    waspCliBuild,
    waspCliNewMinimalStarter,
  )

waspBuildGoldenTest :: GoldenTest
waspBuildGoldenTest =
  makeGoldenTest "wasp-build" $
    sequence
      [ waspCliNewMinimalStarter,
        cdIntoCurrentProject,
        setWaspDbToPSQL,
        waspCliBuild,
        buildDockerImage
      ]
