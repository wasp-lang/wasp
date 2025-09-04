module GoldenTests.KitchenSink (kitchenSinkGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( copyWaspAppGitTrackedFiles,
  )

kitchenSinkGoldenTest :: GoldenTest
kitchenSinkGoldenTest =
  makeGoldenTest "kitchen-sink" $
    sequence
      [copyWaspAppGitTrackedFiles "waspc/examples/todoApp"]
