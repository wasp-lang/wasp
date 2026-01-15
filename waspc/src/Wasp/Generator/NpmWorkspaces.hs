module Wasp.Generator.NpmWorkspaces
  ( serverPackageName,
    webAppPackageName,
    requiredWorkspaceGlobs,
  )
where

import Data.Either (fromRight)
import Data.Set (Set, fromList)
import StrongPath (Dir, Path', Rel, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP
import Wasp.Generator.SdkGenerator.Common (sdkRootDirInProjectRootDir)
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
  )

-- | Returns the set of workspaces that should be included in the user's `package.json` file. Each
-- workspace is a glob that matches all packages in a certain directory.
--
-- The glob syntax is POSIX-path-like, but not actually a path, so it's represented as a String.
requiredWorkspaceGlobs :: Set String
requiredWorkspaceGlobs =
  fromList
    [ makeGlobForAllSubdirs $ dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir,
      makeGlobForDir $ dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> sdkRootDirInProjectRootDir
    ]
  where
    makeGlobForAllSubdirs :: Path' (Rel WaspProjectDir) (Dir a) -> String
    makeGlobForAllSubdirs dir = makeGlobForDir dir FP.</> "*"

    makeGlobForDir :: Path' (Rel WaspProjectDir) (Dir a) -> String
    makeGlobForDir inputDir =
      FP.dropTrailingPathSeparator $
        SP.fromRelDirP $
          fromRight (makeNonPosixError inputDir) $
            SP.relDirToPosix inputDir

    makeNonPosixError inputDir =
      error $
        "This should never happen: our paths should always be POSIX-compatible, but they're not. (Received: "
          ++ show inputDir
          ++ ")"

serverPackageName :: String
serverPackageName = workspacePackageName "server"

webAppPackageName :: String
webAppPackageName = workspacePackageName "webapp"

workspacePackageName :: String -> String
workspacePackageName baseName = "@wasp.sh/generated-" ++ baseName
