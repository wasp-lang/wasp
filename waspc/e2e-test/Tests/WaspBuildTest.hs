module Tests.WaspBuildTest (waspBuild) where

import GoldenTest (GoldenTest, runGoldenTest)
import ShellCommands (cdIntoCurrentProject, combineMakeShellCommands, waspCliBuild, waspCliNew)

waspBuild :: GoldenTest
waspBuild = do
  let makeShellCommand =
        combineMakeShellCommands
          [ waspCliNew,
            cdIntoCurrentProject,
            waspCliBuild
          ]

  runGoldenTest "waspBuild" makeShellCommand
