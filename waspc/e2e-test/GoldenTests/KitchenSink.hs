module GoldenTests.KitchenSink (kitchenSinkGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    cdIntoCurrentProject,
    copyGitTrackedFilesFromRepo,
    dockerBuild,
    waspCliBuild,
    waspCliCompile,
  )

kitchenSinkGoldenTest :: GoldenTest
kitchenSinkGoldenTest =
  makeGoldenTest "kitchen-sink" $
    sequence
      [ copyGitTrackedFilesFromRepo "waspc/examples/todoApp",
        cdIntoCurrentProject,
        copyEnvServerHeadless,
        waspCliCompile,
        waspCliBuild,
        dockerBuild
      ]

copyEnvServerHeadless :: ShellCommandBuilder ShellCommand
copyEnvServerHeadless = return "cp .env.server.headless .env.server"
