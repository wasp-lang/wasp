module Wasp.PackageManager.MarkerFiles
  ( MarkerFile (..),
    PackageRootDir,
    lockfilesAndWorkspaceFiles,
    installMetadataFiles,
  )
where

import StrongPath (File', Path', Rel, relfile)
import Wasp.PackageManager.PackageManager (PackageManager (..))

-- | The root directory of a package, where its package.json lives. That is
-- the directory detection starts in, or, when it starts in a package inside a
-- monorepo, the workspace root above it.
data PackageRootDir

-- | A file whose presence in a package root tells which package manager
-- the project uses.
data MarkerFile = MarkerFile
  { path :: Path' (Rel PackageRootDir) File',
    packageManager :: PackageManager
  }

-- | Committed files, so they work on a fresh clone. When several exist, the
-- first one wins. A stray @package-lock.json@ next to @pnpm-lock.yaml@ usually
-- comes from someone running @npm install@ by mistake.
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
