module Wasp.Generator.WebAppGenerator
  ( webAppRootDirInProjectRootDir,
    viteBuildDirInWebAppDir,
    createWebAppRootDir,
  )
where

import StrongPath (Abs, Dir, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing)
import Wasp.Generator.Common (ProjectRootDir)

data WebAppRootDir

data WebAppViteBuildDir

-- We require the `web-app` dir to be generated for deployment purposes.
-- Our deployment tooling expects to have a folder outside of the user project
-- dir where e.g. Dockerfile for static server or Staticfile can be created.
webAppRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir WebAppRootDir)
webAppRootDirInProjectRootDir = [reldir|web-app|]

viteBuildDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppViteBuildDir)
viteBuildDirInWebAppDir = [reldir|build|]

createWebAppRootDir :: Path' Abs (Dir ProjectRootDir) -> IO ()
createWebAppRootDir projectDir = createDirectoryIfMissing True webAppRootDir
  where
    webAppRootDir = SP.fromAbsDir $ projectDir </> webAppRootDirInProjectRootDir
