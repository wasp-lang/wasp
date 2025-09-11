module GoldenTests.KitchenSinkGoldenTest (kitchenSinkGoldenTest) where

import GoldenTest (GoldenTest, makeGoldenTest)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    cdIntoGoldenTestProject,
    copyContentsOfGitTrackedDirToGoldenTestProject,
    waspCliCompile,
  )
import StrongPath (reldir)

kitchenSinkGoldenTest :: GoldenTest
kitchenSinkGoldenTest =
  makeGoldenTest "kitchen-sink" $
    sequence
      [ copyContentsOfGitTrackedDirToGoldenTestProject [reldir|waspc/examples/todoApp/|],
        cdIntoGoldenTestProject $
          sequence
            [ createDotEnvServerFile,
              waspCliCompile
            ]
      ]
  where
    createDotEnvServerFile :: ShellCommandBuilder ctx ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"
