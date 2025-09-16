module Tests.KitchenSinkGoldenTest (kitchenSinkGoldenTest) where

import GoldenTest.Runner (GoldenTest, makeGoldenTest)
import GoldenTest.ShellCommands
  ( copyContentsOfGitTrackedDirToGoldenTestWaspAppDir,
    withInGoldenTestWaspAppDir,
  )
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
  )
import StrongPath (reldir)
import WaspApp.ShellCommands
  ( WaspAppContext,
    waspCliCompile,
  )

kitchenSinkGoldenTest :: GoldenTest
kitchenSinkGoldenTest =
  makeGoldenTest
    "kitchen-sink"
    [ copyContentsOfGitTrackedDirToGoldenTestWaspAppDir [reldir|waspc/examples/todoApp/|],
      withInGoldenTestWaspAppDir
        [ createDotEnvServerFile,
          waspCliCompile
        ]
    ]
  where
    createDotEnvServerFile :: ShellCommandBuilder WaspAppContext ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"
