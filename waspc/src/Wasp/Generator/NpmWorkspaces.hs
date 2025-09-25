module Wasp.Generator.NpmWorkspaces
  ( serverPackageName,
    toWorkspacesField,
    webAppPackageName,
    workspaces,
  )
where

import Control.Exception (Exception (displayException))
import Data.List (sort)
import StrongPath (Dir, Path, Posix, Rel, fromRelDirP, relDirToPosix, reldirP, (</>))
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec, isBuild)
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

-- | Returns the list of workspaces that should be included in the generated package.json file. Each
-- workspace is a glob that matches all packages in a certain directory.
-- The path syntax here is always POSIX, because Windows syntax does not allow globs, and `npm`
-- prefers it.
--
-- Order doesn't matter, but we sort the packages to ensure a deterministic order. Otherwise, we
-- might inadvertently change the order and the compiler will complain about it in user's projects.
workspaces :: [Path Posix (Rel WaspProjectDir) (Dir ())]
workspaces =
  sort
    [ globFor generatedCodeDirInDotWaspDir,
      globFor buildDirInDotWaspDir
      -- TODO: Add SDK as a workspace:
      -- Currently a overly-zealous resolution makes an incompatible resolution for `@types/react`
      -- that would make the workspace installation fail.
      -- Review when we upgrade React 19 (#2482).
    ]
  where
    globFor dir =
      forceRelDirToPosix (dotWaspDirInWaspProjectDir </> dir)
        </> packageWildcard
    packageWildcard = [reldirP|*|]

    forceRelDirToPosix = either nonPosixError id . relDirToPosix
    nonPosixError exception =
      error $
        unlines
          [ "This should never happen: our paths should always be POSIX-compatible, but they're not.",
            displayException exception
          ]

toWorkspacesField :: [Path Posix (Rel WaspProjectDir) (Dir ())] -> [String]
toWorkspacesField =
  -- While the trailing slashes do not matter, we drop them because they will be user-visible in
  -- their `package.json`, and it is more customary without them.
  fmap (FP.Posix.dropTrailingPathSeparator . fromRelDirP)

serverPackageName :: AppSpec -> String
serverPackageName = workspacePackageName "server"

webAppPackageName :: AppSpec -> String
webAppPackageName = workspacePackageName "webapp"

workspacePackageName :: String -> AppSpec -> String
workspacePackageName baseName spec = "@wasp.sh/generated-" ++ baseName ++ "-" ++ mode
  where
    mode = if isBuild spec then "build" else "dev"
