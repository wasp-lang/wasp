module Wasp.Generator.NpmWorkspaces
  ( serverPackageName,
    webAppPackageName,
    workspaces,
  )
where

import Data.List (sort)
import StrongPath (Dir, Path', Rel, reldir, (</>))
import Wasp.AppSpec (AppSpec, isBuild)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Project.Common (WaspProjectDir, buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

-- TODO: Add SDK as a workspace:
-- Currently a overly-zealous resolution makes an incompatible resolution for `@types/react` that
-- would make the workspace installation fail.
-- Review when we upgrade React 19 (#2482).
workspaces :: [Path' (Rel WaspProjectDir) (Dir ())]
workspaces =
  -- Order doesn't matter. But we sort the packages to ensure a deterministic order. Otherwise, we
  -- might inadvertently change the order and the compiler will complain about it in user's
  -- projects.
  sort
    [ generatedCodeDirWorkspaceGlob,
      buildDirWorkspaceGlob
    ]
  where
    generatedCodeDirWorkspaceGlob = dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> packageWildcard
    buildDirWorkspaceGlob = dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir </> packageWildcard

    packageWildcard :: Path' (Rel ProjectRootDir) (Dir ())
    packageWildcard = [reldir|*|]

serverPackageName :: AppSpec -> String
serverPackageName = workspacePackageName "server"

webAppPackageName :: AppSpec -> String
webAppPackageName = workspacePackageName "webapp"

workspacePackageName :: String -> AppSpec -> String
workspacePackageName baseName spec = "@wasp.sh/generated-" ++ baseName ++ "-" ++ mode
  where
    mode = if isBuild spec then "build" else "dev"
