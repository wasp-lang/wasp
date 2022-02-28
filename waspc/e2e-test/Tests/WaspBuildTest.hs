module Tests.WaspBuildTest (waspBuild) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    reformatPackageJson,
    setDbToPSQL,
    waspCliBuild,
    waspCliNew,
  )

waspBuild :: GoldenTest
waspBuild = do
  let commands =
        sequence
          [ waspCliNew,
            cdIntoCurrentProject,
            setDbToPSQL,
            waspCliBuild,
            reformatPackageJson "build"
          ]

  makeGoldenTest "waspBuild" commands
