module Tests.SnapshotTests.AuthProviderExternalSnapshotTest (authProviderExternalSnapshotTest) where

import ShellCommands
  ( copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
    inSnapshotWaspProjectDir,
    waspCliCompile,
    waspCliInstall,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import StrongPath (reldir)

-- | Snapshots an app whose auth runs through an external provider (the Clerk
-- example, whose adapter is hand-written app code).
--
-- This is the golden proof of capability-gated code generation: the snapshot
-- must contain no auth forms, no password\/lucia\/jwt modules, no JWT_SECRET,
-- and no lucia dependencies -- and must contain the @wasp\/auth\/provider@
-- identity module and the manifest's env vars in the generated env schemas.
authProviderExternalSnapshotTest :: SnapshotTest
authProviderExternalSnapshotTest =
  makeSnapshotTest
    "auth-provider-external"
    [ copyContentsOfGitTrackedDirToSnapshotWaspProjectDir [reldir|examples/auth-providers/clerk|],
      inSnapshotWaspProjectDir
        [ waspCliInstall,
          waspCliCompile
        ]
    ]
