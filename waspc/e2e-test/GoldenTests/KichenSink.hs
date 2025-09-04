module GoldenTests.KitchenSink (kitchenSinkGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( copyKitchenSinkGitTrackedFiles,
  )

kitchenSinkGoldenTest :: GoldenTest
kitchenSinkGoldenTest =
  makeGoldenTest "kitchen-sink" $
    sequence
      [copyKitchenSinkGitTrackedFiles]
