module Wasp.Generator.WebAppGenerator
  ( webAppRootDirInGeneratedAppDir,
    viteBuildDirPath,
    WebAppViteBuildDir,
  )
where

import StrongPath (Dir, Path', Rel, reldir, (</>))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedAppDirInDotWaspDir,
  )

data WebAppRootDir

data WebAppViteBuildDir

webAppRootDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir WebAppRootDir)
webAppRootDirInGeneratedAppDir = [reldir|web-app|]

viteBuildDirPath :: Path' (Rel WaspProjectDir) (Dir WebAppViteBuildDir)
viteBuildDirPath =
  dotWaspDirInWaspProjectDir
    </> generatedAppDirInDotWaspDir
    </> webAppRootDirInGeneratedAppDir
    </> viteBuildDirInWebAppDir

viteBuildDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppViteBuildDir)
viteBuildDirInWebAppDir = [reldir|build|]
