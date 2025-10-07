module Wasp.Generator.NpmWorkspaces
  ( serverPackageName,
    toWorkspacesField,
    webAppPackageName,
    workspaces,
  )
where

import Control.Exception (Exception (displayException))
import StrongPath (Dir, Dir', Path, Posix, Rel, fromRelDirP, relDirToPosix, reldirP, (</>))
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec, isBuild)
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

-- | Returns the list of workspaces that should be included in the generated package.json file. Each
-- workspace is a glob that matches all packages in a certain directory.
-- The path syntax here is always POSIX, because Windows syntax does not allow globs, and `npm`
-- prefers it.
workspaces :: [Path Posix (Rel WaspProjectDir) Dir']
workspaces =
  [ globFor generatedCodeDirInDotWaspDir,
    globFor buildDirInDotWaspDir
    -- TODO: Add SDK as a workspace (#3233)
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
  map (FP.Posix.dropTrailingPathSeparator . fromRelDirP)

serverPackageName :: AppSpec -> String
serverPackageName = workspacePackageName "server"

webAppPackageName :: AppSpec -> String
webAppPackageName = workspacePackageName "webapp"

workspacePackageName :: String -> AppSpec -> String
workspacePackageName baseName spec = "@wasp.sh/generated-" ++ baseName ++ "-" ++ modeName
  where
    modeName
      | isBuild spec = "build"
      | otherwise = "dev"
