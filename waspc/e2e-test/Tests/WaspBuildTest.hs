module Tests.WaspBuildTest (waspBuild) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    dockerBuild,
    setDbToPSQL,
    waspCliBuild,
    waspCliNew,
  )

waspBuild :: GoldenTest
waspBuild =
  makeGoldenTest "waspBuild" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject,
        setDbToPSQL,
        waspCliBuild,
        dockerBuild
      ]
