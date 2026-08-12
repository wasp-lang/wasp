module Wasp.Generator.WebAppGenerator
  ( webAppRootDirInGeneratedAppDir,
    createWebAppRootDir,
    webAppRootDirPath,
    viteBuildDirPath,
    nitroServerEntryInGeneratedAppDir,
    WebAppViteBuildDir,
  )
where

import StrongPath (Abs, Dir, File', Path, Path', Posix, Rel, reldir, relfileP, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedAppDirInDotWaspDir,
  )

data WebAppRootDir

data WebAppViteBuildDir

-- We require the `web-app` dir to be generated for deployment purposes.
-- Our deployment tooling expects to have a folder outside of the user project
-- dir where e.g. Dockerfile for static server or Staticfile can be created.
webAppRootDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir WebAppRootDir)
webAppRootDirInGeneratedAppDir = [reldir|web-app|]

-- | Where Nitro writes the built client app (its `output.dir`): the server
-- bundle goes into a `server` subdirectory, the static assets into
-- 'viteBuildDirInWebAppDir'.
webAppRootDirPath :: Path' (Rel WaspProjectDir) (Dir WebAppRootDir)
webAppRootDirPath =
  dotWaspDirInWaspProjectDir
    </> generatedAppDirInDotWaspDir
    </> webAppRootDirInGeneratedAppDir

-- | Where the client's static assets end up (Nitro's `output.publicDir`).
-- Our deployment tooling reads the client artefacts from here.
viteBuildDirPath :: Path' (Rel WaspProjectDir) (Dir WebAppViteBuildDir)
viteBuildDirPath = webAppRootDirPath </> viteBuildDirInWebAppDir

viteBuildDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppViteBuildDir)
viteBuildDirInWebAppDir = [reldir|build|]

-- | The entry point of the server Nitro builds. It serves the whole app: the
-- client's static assets and prerendered pages, and the server's API.
--
-- A POSIX path because it ends up in npm scripts and Dockerfiles, which use
-- forward slashes on every platform.
nitroServerEntryInGeneratedAppDir :: Path Posix (Rel GeneratedAppDir) File'
nitroServerEntryInGeneratedAppDir = [relfileP|web-app/server/index.mjs|]

createWebAppRootDir :: Path' Abs (Dir GeneratedAppDir) -> IO ()
createWebAppRootDir generatedAppDir = createDirectoryIfMissing True webAppRootDir
  where
    webAppRootDir = SP.fromAbsDir $ generatedAppDir </> webAppRootDirInGeneratedAppDir
