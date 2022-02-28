module Tests.WaspBuildTest (waspBuild) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( OutputDir (BuildOutputDir),
    cdIntoCurrentProject,
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
            reformatPackageJson BuildOutputDir
          ]

  makeGoldenTest "waspBuild" commands
