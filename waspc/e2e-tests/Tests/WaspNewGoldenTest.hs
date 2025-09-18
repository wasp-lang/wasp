module Tests.WaspNewGoldenTest (waspNewGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import GoldenTest.ShellCommands (createGoldenTestWaspApp)

waspNewGoldenTest :: GoldenTest
waspNewGoldenTest =
  makeGoldenTest
    "wasp-new"
    [createGoldenTestWaspApp]
