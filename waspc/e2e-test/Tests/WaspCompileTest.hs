module Tests.WaspCompileTest (waspCompile) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    waspCliCompile,
    waspCliNew,
  )

waspCompile :: GoldenTest
waspCompile = do
  let commands =
        sequence
          [ waspCliNew,
            cdIntoCurrentProject,
            waspCliCompile
          ]

  makeGoldenTest "waspCompile" commands
