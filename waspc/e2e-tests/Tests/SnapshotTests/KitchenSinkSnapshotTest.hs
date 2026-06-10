module Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import SnapshotTest (SnapshotTest, makeSnapshotTest)
import Steps
  ( appendToFile,
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
    copyFile,
    inSnapshotWaspProjectDir,
    runCommand,
    waspCliCompile,
    waspCliInstall,
  )
import StrongPath (reldir)

kitchenSinkSnapshotTest :: SnapshotTest
kitchenSinkSnapshotTest =
  makeSnapshotTest
    "kitchen-sink"
    [ copyContentsOfGitTrackedDirToSnapshotWaspProjectDir [reldir|examples/kitchen-sink|],
      inSnapshotWaspProjectDir
        [ createDotEnvServerFile,
          normalizePostgresConnectionString,
          runCommand waspCliInstall,
          runCommand waspCliCompile
        ]
    ]
  where
    createDotEnvServerFile = copyFile ".env.server.example" ".env.server"
    -- NOTE: The leading newline is kept from the previous, `printf`-based
    -- version of this step, since `.env.server` is part of the golden snapshot.
    normalizePostgresConnectionString = appendToFile ".env.server" "\nDATABASE_URL=mock-database-url"
