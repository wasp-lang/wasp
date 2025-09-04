module GoldenTests.MinimalStarter (minimalStarterGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( WaspStarter (Minimal),
    cdIntoCurrentProject,
    waspCliCompile,
    waspCliNewStarter,
  )

minimalStarterGoldenTest :: GoldenTest
minimalStarterGoldenTest =
  makeGoldenTest "minimal-starter" $
    sequence
      [ waspCliNewStarter Minimal,
        cdIntoCurrentProject,
        waspCliCompile
      ]
