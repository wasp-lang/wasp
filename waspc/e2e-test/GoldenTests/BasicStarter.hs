module GoldenTests.BasicStarter (basicStarterGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( WaspStarter (Basic),
    waspCliNewStarter,
  )

basicStarterGoldenTest :: GoldenTest
basicStarterGoldenTest = makeGoldenTest "basic-starter" (pure <$> waspCliNewStarter Basic)
