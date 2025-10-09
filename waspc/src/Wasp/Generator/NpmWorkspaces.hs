module Wasp.Generator.NpmWorkspaces
  ( serverPackageName,
    webAppPackageName,
    workspaceGlobs,
  )
where

import Data.Either (fromRight)
import StrongPath (Dir, Path, Path', Posix, Rel, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec, isBuild)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Project.Common
  ( WaspProjectDir,
    buildDirInDotWaspDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
  )

-- | Returns the list of workspaces that should be included in the user's `package.json` file. Each
-- workspace is a glob that matches all packages in a certain directory.
--
-- The glob syntax is POSIX-path-like, but not actually a path, so it's represented as a String.
workspaceGlobs :: [String]
workspaceGlobs =
  FP.Posix.dropTrailingPathSeparator . SP.fromRelDirP
    <$> [ makeGlobFromProjectRoot $ dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir,
          makeGlobFromProjectRoot $ dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir
          -- TODO: Add SDK as a workspace (#3233)
        ]
  where
    makeGlobFromProjectRoot :: Path' (Rel WaspProjectDir) (Dir ProjectRootDir) -> Path Posix (Rel WaspProjectDir) (Dir a)
    makeGlobFromProjectRoot projectRootDir = (forceRelDirToPosix projectRootDir) </> packageWildcard

    -- We force this to be POSIX because Windows-style paths do not accept wildcard characters.
    packageWildcard = [SP.reldirP|*|]

    forceRelDirToPosix inputDir = fromRight (makeNonPosixError inputDir) $ SP.relDirToPosix inputDir
    makeNonPosixError inputDir =
      error $
        "This should never happen: our paths should always be POSIX-compatible, but they're not. (Received: "
          ++ show inputDir
          ++ ")"

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
