module Tests.WaspCompileGoldenTest (waspCompileGoldenTest) where

import GoldenTest.Runner (GoldenTest, makeGoldenTest)
import GoldenTest.ShellCommands
  ( createGoldenTestWaspApp,
    withInGoldenTestWaspAppDir,
  )
import WaspApp.ShellCommands
  ( waspCliCompile,
  )

waspCompileGoldenTest :: GoldenTest
waspCompileGoldenTest =
  makeGoldenTest
    "wasp-compile"
    [ createGoldenTestWaspApp,
      withInGoldenTestWaspAppDir
        [waspCliCompile]
    ]
