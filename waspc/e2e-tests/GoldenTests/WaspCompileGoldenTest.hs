module GoldenTests.WaspCompileGoldenTest (waspCompileGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( cdIntoGoldenTestProject,
    waspCliCompile,
    waspCliNewMinimalStarter,
  )

waspCompileGoldenTest :: GoldenTest
waspCompileGoldenTest =
  makeGoldenTest "wasp-compile" $
    sequence
      [ waspCliNewMinimalStarter "wasp-compile",
        cdIntoGoldenTestProject $
          sequence [waspCliCompile]
      ]
