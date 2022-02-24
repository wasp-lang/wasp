module Tests.WaspNewTest (waspNew) where

import GoldenTest (GoldenTest (GoldenTest, _goldenTestName, _makeShellCommand))
import ShellCommands
  ( waspCliNew,
  )

waspNew :: GoldenTest
waspNew = GoldenTest {_goldenTestName = "waspNew", _makeShellCommand = waspCliNew}
