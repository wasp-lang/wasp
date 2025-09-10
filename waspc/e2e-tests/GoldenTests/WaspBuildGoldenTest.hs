module GoldenTests.WaspBuildGoldenTest (waspBuildGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    dockerBuild,
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
        dockerBuild
      ]
