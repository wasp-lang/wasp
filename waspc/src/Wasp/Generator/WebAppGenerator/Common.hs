module Wasp.Generator.WebAppGenerator.Common
  ( WebAppRootDir,
    WebAppViteBuildDir,
    webAppRootDirInGeneratedAppDir,
    viteBuildDirInWebAppDir,
    viteAssetsDirName,
    getBaseDir,
    getBaseDirPathPrefix,
    serverUrlEnvVarName,
    devProxyTargetEnvVarName,
    clientPortEnvVarName,
  )
where

import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import StrongPath (Abs, Dir, Path, Path', Posix, Rel, absdirP, reldir)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (GeneratedAppDir)

data WebAppRootDir

data WebAppViteBuildDir

-- We require the `web-app` dir to be generated for deployment purposes.
-- Our deployment tooling expects to have a folder outside of the user project
-- dir where e.g. Dockerfile for static server or Staticfile can be created.
webAppRootDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir WebAppRootDir)
webAppRootDirInGeneratedAppDir = [reldir|web-app|]

viteBuildDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppViteBuildDir)
viteBuildDirInWebAppDir = [reldir|build|]

-- | The dir inside the Vite build where Vite puts the files it fingerprints (`build.assetsDir`).
viteAssetsDirName :: String
viteAssetsDirName = "assets"

getBaseDir :: AppSpec -> Path Posix Abs (Dir ())
getBaseDir spec = fromMaybe [absdirP|/|] maybeBaseDir
  where
    maybeBaseDir = SP.parseAbsDirP =<< (AS.App.Client.baseDir =<< AS.App.client (snd $ getApp spec))

-- | The client base dir as a path prefix: "" for the root, otherwise without a trailing slash
-- (e.g. "/my-app"), so it can go in front of a path or after an origin as it is.
getBaseDirPathPrefix :: AppSpec -> String
getBaseDirPathPrefix = dropWhileEnd (== '/') . SP.fromAbsDirP . getBaseDir

serverUrlEnvVarName :: String
serverUrlEnvVarName = "REACT_APP_API_URL"

-- | In single deployment mode the Vite dev server proxies server requests to this URL.
-- Users can reuse it in their own `server.proxy` entries in `vite.config.ts`.
devProxyTargetEnvVarName :: String
devProxyTargetEnvVarName = "WASP_DEV_PROXY_TARGET"

clientPortEnvVarName :: String
clientPortEnvVarName =
  -- Not prefixed with `WASP_` because many deployment platforms use this env.
  "PORT"
