module Tests.WaspBuildTest (waspBuild) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
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
            waspCliBuild
          ]

  makeGoldenTest "waspBuild" commands
