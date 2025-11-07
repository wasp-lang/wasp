module Tests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import SnapshotTest.TestCommands
  ( copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
    withInSnapshotWaspProjectDir,
  )
import StrongPath (reldir)
import TestCommands
  ( TestCommand,
    shellCommand,
  )
import WaspProject.TestCommands
  ( WaspProjectContext,
    waspCliCompile,
  )

kitchenSinkSnapshotTest :: SnapshotTest
kitchenSinkSnapshotTest =
  makeSnapshotTest
    "kitchen-sink"
    [ copyContentsOfGitTrackedDirToSnapshotWaspProjectDir [reldir|examples/kitchen-sink/|],
      withInSnapshotWaspProjectDir
        [ createDotEnvServerFile,
          normalizePostgresConnectionString,
          waspCliCompile
        ]
    ]
  where
    createDotEnvServerFile :: TestCommand WaspProjectContext ()
    createDotEnvServerFile = shellCommand "cp .env.server.example .env.server"

    normalizePostgresConnectionString :: TestCommand WaspProjectContext ()
    normalizePostgresConnectionString = shellCommand "printf '\\nDATABASE_URL=mock-database-url\\n' >> .env.server"
