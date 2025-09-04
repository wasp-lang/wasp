module GoldenTests.MinimalStarter (minimalStarterGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( WaspStarter (Minimal),
    waspCliNewStarter,
  )

minimalStarterGoldenTest :: GoldenTest
minimalStarterGoldenTest = makeGoldenTest "minimal-starter" (pure <$> waspCliNewStarter Minimal)
