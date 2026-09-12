module Wasp.Generator.WebAppGenerator
  ( webAppRootDirInGeneratedAppDir,
    createWebAppRootDir,
    viteBuildDirPath,
    WebAppViteBuildDir,
  )
where

import StrongPath (Abs, Dir, Path', Rel, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.WebAppGenerator.Common
  ( WebAppViteBuildDir,
    viteBuildDirInWebAppDir,
    webAppRootDirInGeneratedAppDir,
  )
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedAppDirInDotWaspDir,
  )

viteBuildDirPath :: Path' (Rel WaspProjectDir) (Dir WebAppViteBuildDir)
viteBuildDirPath =
  dotWaspDirInWaspProjectDir
    </> generatedAppDirInDotWaspDir
    </> webAppRootDirInGeneratedAppDir
    </> viteBuildDirInWebAppDir

createWebAppRootDir :: Path' Abs (Dir GeneratedAppDir) -> IO ()
createWebAppRootDir generatedAppDir = createDirectoryIfMissing True webAppRootDir
  where
    webAppRootDir = SP.fromAbsDir $ generatedAppDir </> webAppRootDirInGeneratedAppDir
