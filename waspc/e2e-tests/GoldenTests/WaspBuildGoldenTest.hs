module GoldenTests.WaspBuildGoldenTest (waspBuildGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( buildDockerImage,
    cdIntoCurrentProject,
    setDbToPSQL,
    waspCliBuild,
    waspCliNewMinimalStarter,
  )

waspBuildGoldenTest :: GoldenTest
waspBuildGoldenTest =
  makeGoldenTest "wasp-build" $
    sequence
      [ waspCliNewMinimalStarter,
        cdIntoCurrentProject,
        setDbToPSQL,
        waspCliBuild,
        buildDockerImage
      ]
