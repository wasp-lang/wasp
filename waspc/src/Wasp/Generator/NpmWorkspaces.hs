module Wasp.Generator.NpmWorkspaces
  ( serverPackageName,
    webAppPackageName,
    workspaceGlobs,
  )
where

import Data.Either (fromRight)
import Data.Set (Set, fromList)
import StrongPath (Dir, Path', Rel, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP
import Wasp.AppSpec (AppSpec, isBuild)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Project.Common
  ( WaspProjectDir,
    buildDirInDotWaspDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
  )

-- | Returns the set of workspaces that should be included in the user's `package.json` file. Each
-- workspace is a glob that matches all packages in a certain directory.
--
-- The glob syntax is POSIX-path-like, but not actually a path, so it's represented as a String.
workspaceGlobs :: Set String
workspaceGlobs =
  fromList
    [ makeGlobFromProjectRoot $ dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir,
      makeGlobFromProjectRoot $ dotWaspDirInWaspProjectDir </> buildDirInDotWaspDir
      -- TODO: Add SDK as a workspace (#3233)
    ]
  where
    makeGlobFromProjectRoot :: Path' (Rel WaspProjectDir) (Dir ProjectRootDir) -> String
    makeGlobFromProjectRoot projectRootDir =
      relDirToPosixString projectRootDir FP.</> "*"

    relDirToPosixString inputDir =
      SP.fromRelDirP $ fromRight (makeNonPosixError inputDir) $ SP.relDirToPosix inputDir

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
