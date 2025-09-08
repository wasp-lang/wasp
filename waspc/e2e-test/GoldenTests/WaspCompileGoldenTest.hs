module GoldenTests.WaspCompileGoldenTest (waspCompileGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoCurrentProject,
    waspCliCompile,
    waspCliNewMinimalStarter,
  )

waspCompileGoldenTest :: GoldenTest
waspCompileGoldenTest =
  makeGoldenTest "wasp-compile" $
    sequence
      [ waspCliNewMinimalStarter,
        cdIntoCurrentProject,
        waspCliCompile
      ]
