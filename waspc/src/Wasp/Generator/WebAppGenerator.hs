module Wasp.Generator.WebAppGenerator
  ( genWebApp,
    webAppRootDirInGeneratedAppDir,
    viteBuildDirPath,
    WebAppViteBuildDir,
  )
where

import StrongPath (Dir, Path', Rel, reldir, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.App (getDeploymentMode)
import qualified Wasp.AppSpec.App.Deployment as Deployment
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createEmptyDirFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedAppDirInDotWaspDir,
  )

data WebAppRootDir

data WebAppViteBuildDir

-- Vite builds the client into this dir. Our deployment tooling also uses it
-- as a folder outside of the user project dir where e.g. a Dockerfile for a
-- static server can be created.
webAppRootDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir WebAppRootDir)
webAppRootDirInGeneratedAppDir = [reldir|web-app|]

-- | A production build that deploys the client separately from the server
-- comes with the dir that the client deployment lives in.
genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec
  | AS.isProduction spec && isSplitDeployment =
      return [createEmptyDirFileDraft webAppRootDirInGeneratedAppDir]
  | otherwise = return []
  where
    isSplitDeployment = getDeploymentMode (getApp spec) == Deployment.Split

viteBuildDirPath :: Path' (Rel WaspProjectDir) (Dir WebAppViteBuildDir)
viteBuildDirPath =
  dotWaspDirInWaspProjectDir
    </> generatedAppDirInDotWaspDir
    </> webAppRootDirInGeneratedAppDir
    </> viteBuildDirInWebAppDir

viteBuildDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppViteBuildDir)
viteBuildDirInWebAppDir = [reldir|build|]
