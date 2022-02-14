module Tests.WaspNewTest (waspNew) where

import GoldenTest (GoldenTest, runGoldenTest)
import ShellCommands (waspCliNew)

waspNew :: GoldenTest
waspNew = runGoldenTest "waspNew" waspCliNew
