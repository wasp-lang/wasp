module GoldenTests.MinimalStarter (minimalStarterGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( waspCliNewMinimalStarter,
  )

minimalStarterGoldenTest :: GoldenTest
minimalStarterGoldenTest =
  makeGoldenTest "minimal-starter" $
    sequence
      [waspCliNewMinimalStarter]
