module Tests.WaspNewGoldenTest (waspNewGoldenTest) where

import GoldenTest.Runner (GoldenTest, makeGoldenTest)
import GoldenTest.ShellCommands (createGoldenTestWaspApp)

waspNewGoldenTest :: GoldenTest
waspNewGoldenTest =
  makeGoldenTest
    "wasp-new"
    [createGoldenTestWaspApp]
