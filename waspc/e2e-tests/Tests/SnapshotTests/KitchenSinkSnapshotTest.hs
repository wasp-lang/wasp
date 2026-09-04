module Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    WaspProjectContext,
    copyContentsOfGitTrackedDirToSnapshotDir,
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
    inSnapshotWaspProjectDir,
    waspCliCompile,
    waspCliInstall,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import StrongPath (reldir)

kitchenSinkSnapshotTest :: SnapshotTest
kitchenSinkSnapshotTest =
  makeSnapshotTest
    "kitchen-sink"
    [ -- The app depends on `@wasp.sh/auth` (Wasp's own auth as a package) via a
      -- relative file: dependency, so the packages dir has to sit where the
      -- app expects it.
      copyContentsOfGitTrackedDirToSnapshotDir [reldir|examples/auth-providers/packages|] "auth-providers/packages",
      copyContentsOfGitTrackedDirToSnapshotWaspProjectDir [reldir|examples/kitchen-sink|],
      inSnapshotWaspProjectDir
        [ createDotEnvServerFile,
          normalizePostgresConnectionString,
          waspCliInstall,
          waspCliCompile
        ]
    ]
  where
    createDotEnvServerFile :: ShellCommandBuilder WaspProjectContext ShellCommand
    createDotEnvServerFile = return "cp .env.server.example .env.server"

    normalizePostgresConnectionString :: ShellCommandBuilder WaspProjectContext ShellCommand
    normalizePostgresConnectionString = return "printf '\\nDATABASE_URL=mock-database-url\\n' >> .env.server"
