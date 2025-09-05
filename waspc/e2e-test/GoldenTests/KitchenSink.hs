module GoldenTests.KitchenSink (kitchenSinkGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    cdIntoCurrentProject,
    copyGitTrackedFilesFromRepoPath,
    dockerBuild,
    waspCliBuild,
    waspCliCompile,
  )
import StrongPath (reldir)

kitchenSinkGoldenTest :: GoldenTest
kitchenSinkGoldenTest =
  makeGoldenTest "kitchen-sink" $
    sequence
      [ copyGitTrackedFilesFromRepoPath [reldir|waspc/examples/todoApp/|],
        cdIntoCurrentProject,
        copyEnvServerHeadless,
        waspCliCompile,
        waspCliBuild,
        dockerBuild
      ]

copyEnvServerHeadless :: ShellCommandBuilder ShellCommand
copyEnvServerHeadless = return "cp .env.server.headless .env.server"
