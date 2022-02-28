module Tests.WaspCompileTest (waspCompile) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    reformatPackageJson,
    waspCliCompile,
    waspCliNew,
  )

waspCompile :: GoldenTest
waspCompile = do
  let commands =
        sequence
          [ waspCliNew,
            cdIntoCurrentProject,
            waspCliCompile,
            reformatPackageJson "out"
          ]

  makeGoldenTest "waspCompile" commands
