module Tests.WaspCompileTest (waspCompile) where

import GoldenTest (GoldenTest, runGoldenTest)
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

  runGoldenTest "waspCompile" makeShellCommand
