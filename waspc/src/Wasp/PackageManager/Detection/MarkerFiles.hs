module Wasp.PackageManager.Detection.MarkerFiles
  ( MarkerFile (..),
    lockfilesAndWorkspaceFiles,
    installMetadataFiles,
  )
where

import StrongPath (File', Path', Rel, relfile)
import Wasp.PackageManager.Detection.Common (PackageRootDir)
import Wasp.PackageManager.PackageManager (PackageManager (..))

-- | A file whose presence in a package root tells which package manager
-- the project uses.
data MarkerFile = MarkerFile
  { path :: Path' (Rel PackageRootDir) File',
    packageManager :: PackageManager
  }

-- | Committed files, so they work on a fresh clone. When several exist, the
-- first one wins.
lockfilesAndWorkspaceFiles :: [MarkerFile]
lockfilesAndWorkspaceFiles =
  [ MarkerFile [relfile|pnpm-lock.yaml|] Pnpm,
    MarkerFile [relfile|pnpm-workspace.yaml|] Pnpm,
    MarkerFile [relfile|yarn.lock|] Yarn,
    MarkerFile [relfile|.yarnrc.yml|] Yarn,
    MarkerFile [relfile|bun.lock|] Bun,
    MarkerFile [relfile|bun.lockb|] Bun,
    MarkerFile [relfile|package-lock.json|] Npm,
    MarkerFile [relfile|npm-shrinkwrap.json|] Npm
  ]

-- | Files the package manager writes while installing. They only exist after
-- an install, but still work when the lockfile isn't committed.
installMetadataFiles :: [MarkerFile]
installMetadataFiles =
  [ MarkerFile [relfile|node_modules/.modules.yaml|] Pnpm,
    MarkerFile [relfile|node_modules/.yarn-state.yml|] Yarn,
    MarkerFile [relfile|node_modules/.yarn_integrity|] Yarn,
    MarkerFile [relfile|.pnp.cjs|] Yarn,
    MarkerFile [relfile|node_modules/.package-lock.json|] Npm
  ]
