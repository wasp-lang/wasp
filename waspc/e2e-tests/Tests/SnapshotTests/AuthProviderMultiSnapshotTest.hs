module Tests.SnapshotTests.AuthProviderMultiSnapshotTest (authProviderMultiSnapshotTest) where

import ShellCommands
  ( copyContentsOfGitTrackedDirToSnapshotDir,
    copyContentsOfGitTrackedDirToSnapshotWaspProjectDir,
    inSnapshotWaspProjectDir,
    waspCliCompile,
    waspCliInstall,
  )
import SnapshotTest (SnapshotTest, makeSnapshotTest)
import StrongPath (reldir)

-- | Snapshots an app running MULTIPLE auth providers at once (the
-- multi-provider example: Wasp's own auth next to the Clerk adapter).
--
-- This is the golden proof of the providers[] rework: the snapshot must
-- contain the UNION of the wasp-auth surface (auth forms, password\/lucia\/jwt
-- modules, method routes) and the external-provider surface (the provider
-- registry with an instantiated adapter, the provider-addressed exchange
-- route, the manifest's env vars) -- plus the pieces only a provider mix
-- exercises: the plural @wasp\/auth\/provider@ identity module, and a
-- provider-restricted page and query (@authRequired: ["wasp"]@,
-- @auth: ["wasp"]@) threaded through routes and route wrappers.
authProviderMultiSnapshotTest :: SnapshotTest
authProviderMultiSnapshotTest =
  makeSnapshotTest
    "auth-provider-multi"
    [ -- The app depends on the local adapter package via a relative file:
      -- dependency, so the packages dir has to sit next to it.
      copyContentsOfGitTrackedDirToSnapshotDir [reldir|examples/auth-providers/packages|] "packages",
      copyContentsOfGitTrackedDirToSnapshotWaspProjectDir [reldir|examples/auth-providers/multi-provider|],
      inSnapshotWaspProjectDir
        [ waspCliInstall,
          waspCliCompile
        ]
    ]
