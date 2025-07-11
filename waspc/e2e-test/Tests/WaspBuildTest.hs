module Tests.WaspBuildTest (waspBuild) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    dockerBuild,
    setDbToPSQL,
    waspCliBuild,
    waspCliNewMinimalStarter,
  )

waspBuild :: GoldenTest
waspBuild =
  makeGoldenTest "waspBuild" $
    sequence
      [ waspCliNewMinimalStarter,
        cdIntoCurrentProject,
        setDbToPSQL,
        waspCliBuild,
        dockerBuild
      ]
