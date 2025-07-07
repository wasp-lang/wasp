module Tests.WaspNewTest (waspNew) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( waspCliNewMinimalStarter,
  )

waspNew :: GoldenTest
waspNew = makeGoldenTest "waspNew" (pure <$> waspCliNewMinimalStarter)
