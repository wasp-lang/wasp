module GoldenTests.KitchenSinkGoldenTest (kitchenSinkGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspAppContext,
    copyContentsOfGitTrackedDirToGoldenTestProject,
    waspCliCompile,
    withInSnapshotProjectDir,
  )
import StrongPath (reldir)

kitchenSinkGoldenTest :: GoldenTest
kitchenSinkGoldenTest =
  makeGoldenTest "kitchen-sink" $
    sequence
      [ copyContentsOfGitTrackedDirToGoldenTestProject [reldir|waspc/examples/todoApp/|],
        withInSnapshotProjectDir $
          sequence
            [ createDotEnvServerFile,
              waspCliCompile
            ]
      ]
  where
    createDotEnvServerFile :: ShellCommandBuilder WaspAppContext ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"
