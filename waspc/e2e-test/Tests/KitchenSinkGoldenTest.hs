module Tests.KitchenSinkGoldenTest (kitchenSinkGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    cdIntoCurrentProject,
    copyContentsOfGitTrackedDirToCurrentProject,
    waspCliCompile,
  )
import StrongPath (reldir)

kitchenSinkGoldenTest :: GoldenTest
kitchenSinkGoldenTest =
  makeGoldenTest "kitchen-sink" $
    sequence
      [ copyContentsOfGitTrackedDirToCurrentProject [reldir|waspc/examples/todoApp/|],
        cdIntoCurrentProject,
        createDotEnvServerFile,
        waspCliCompile
      ]
  where
    createDotEnvServerFile :: ShellCommandBuilder ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"
