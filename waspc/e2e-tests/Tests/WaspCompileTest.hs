module Tests.WaspCompileTest (waspCompile) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    waspCliCompile,
    waspCliNewMinimalStarter,
  )

waspCompile :: GoldenTest
waspCompile =
  makeGoldenTest "waspCompile" $
    sequence
      [ waspCliNewMinimalStarter,
        cdIntoCurrentProject,
        waspCliCompile
      ]
