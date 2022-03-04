module Tests.WaspCompileTest (waspCompile) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    waspCliCompile,
    waspCliNew,
  )

waspCompile :: GoldenTest
waspCompile =
  makeGoldenTest "waspCompile" $
    sequence
      [ waspCliNew,
        cdIntoCurrentProject,
        waspCliCompile
      ]
