module GoldenTests.KitchenSink (kitchenSinkGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( copyGitTrackedFilesFromRepo,
  )

kitchenSinkGoldenTest :: GoldenTest
kitchenSinkGoldenTest =
  makeGoldenTest "kitchen-sink" $
    sequence
      [ copyGitTrackedFilesFromRepo "waspc/examples/todoApp"
      ]
