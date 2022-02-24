module Tests.WaspCompileTest (waspCompile) where

import GoldenTest (GoldenTest (GoldenTest, _goldenTestName, _makeShellCommand))
import ShellCommands
  ( cdIntoCurrentProject,
    combineMakeShellCommands,
    waspCliCompile,
    waspCliNew,
  )

waspCompile :: GoldenTest
waspCompile = do
  let makeShellCommand =
        combineMakeShellCommands
          [ waspCliNew,
            cdIntoCurrentProject,
            waspCliCompile
          ]

  GoldenTest {_goldenTestName = "waspCompile", _makeShellCommand = makeShellCommand}
