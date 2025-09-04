module GoldenTests.BasicStarter (basicStarterGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( WaspStarter (Basic),
    cdIntoCurrentProject,
    waspCliCompile,
    waspCliNewStarter,
  )

basicStarterGoldenTest :: GoldenTest
basicStarterGoldenTest =
  makeGoldenTest "basic-starter" $
    sequence
      [ waspCliNewStarter Basic,
        cdIntoCurrentProject,
        waspCliCompile
      ]
