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

-- | A production build deploys the client separately from the server, so it
-- comes with the dir that the client deployment lives in.
--
-- NOTE: Once Wasp can serve the client from the server, only builds that
-- deploy them separately should generate this dir.
genWebApp :: AppSpec -> Generator [FileDraft]
genWebApp spec
  | AS.isProduction spec = return [createEmptyDirFileDraft webAppRootDirInGeneratedAppDir]
  | otherwise = return []

viteBuildDirPath :: Path' (Rel WaspProjectDir) (Dir WebAppViteBuildDir)
viteBuildDirPath =
  dotWaspDirInWaspProjectDir
    </> generatedAppDirInDotWaspDir
    </> webAppRootDirInGeneratedAppDir
    </> viteBuildDirInWebAppDir

viteBuildDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppViteBuildDir)
viteBuildDirInWebAppDir = [reldir|build|]
