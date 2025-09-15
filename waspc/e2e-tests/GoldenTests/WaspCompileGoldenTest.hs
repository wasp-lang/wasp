module GoldenTests.WaspCompileGoldenTest (waspCompileGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( waspCliCompile,
    waspCliNewMinimalStarter,
    withInSnapshotProjectDir,
  )

waspCompileGoldenTest :: GoldenTest
waspCompileGoldenTest =
  makeGoldenTest "wasp-compile" $
    sequence
      [ waspCliNewMinimalStarter "wasp-compile",
        withInSnapshotProjectDir $
          sequence [waspCliCompile]
      ]
