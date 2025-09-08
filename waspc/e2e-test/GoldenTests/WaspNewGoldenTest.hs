module GoldenTests.WaspNewGoldenTest (waspNewGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( waspCliNewMinimalStarter,
  )

waspNewGoldenTest :: GoldenTest
waspNewGoldenTest =
  makeGoldenTest "wasp-new" $
    sequence
      [waspCliNewMinimalStarter]
