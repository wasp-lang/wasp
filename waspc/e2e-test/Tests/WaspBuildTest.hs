module Tests.WaspBuildTest (waspBuild) where

import GoldenTest (GoldenTest (GoldenTest, _goldenTestName, _makeShellCommand))
import ShellCommands
  ( cdIntoCurrentProject,
    combineMakeShellCommands,
    setDbToPSQL,
    waspCliBuild,
    waspCliNew,
  )

waspBuild :: GoldenTest
waspBuild = do
  let makeShellCommand =
        combineMakeShellCommands
          [ waspCliNew,
            cdIntoCurrentProject,
            setDbToPSQL,
            waspCliBuild
          ]

  GoldenTest {_goldenTestName = "waspBuild", _makeShellCommand = makeShellCommand}
