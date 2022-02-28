module Tests.WaspNewTest (waspNew) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( waspCliNew,
  )

waspNew :: GoldenTest
waspNew = makeGoldenTest "waspNew" (pure <$> waspCliNew)
