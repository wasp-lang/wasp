module Wasp.Generator.WebAppGenerator.Common
  ( webAppRootDirInProjectRootDir,
    WebAppRootDir,
    WebAppSrcDir,
    getBaseDir,
    getDefaultDevClientUrl,
    defaultClientPort,
    serverUrlEnvVarName,
  )
where

import Data.Maybe (fromMaybe)
import StrongPath (Abs, Dir, Path, Path', Posix, Rel, absdirP, reldir)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.Common (GeneratedSrcDir, ProjectRootDir)

-- | Phantom type representing the web-app root directory in the generated project.
-- After migration to user-land entry point, `.wasp/out/web-app` is primarily used for
-- build output (`.wasp/out/web-app/build`).
data WebAppRootDir

-- | Phantom type representing the web app source directory.
-- This now refers to the user's `src/` directory in the project root, not `.wasp/out/web-app/src`.
data WebAppSrcDir

instance GeneratedSrcDir WebAppSrcDir

-- | Path where web app root dir is located, relative to the root directory of the whole generated project.
-- NOTE: After migration to user-land entry point, this directory is primarily used for build output.
-- The actual source files are now in user-land (src/, index.html, etc.) or SDK.
webAppRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir WebAppRootDir)
webAppRootDirInProjectRootDir = [reldir|web-app|]

getBaseDir :: AppSpec -> Path Posix Abs (Dir ())
getBaseDir spec = fromMaybe [absdirP|/|] maybeBaseDir
  where
    maybeBaseDir = SP.parseAbsDirP =<< (AS.App.Client.baseDir =<< AS.App.client (snd $ getApp spec))

serverUrlEnvVarName :: String
serverUrlEnvVarName = "REACT_APP_API_URL"

defaultClientPort :: Int
defaultClientPort = 3000

getDefaultDevClientUrl :: AppSpec -> String
getDefaultDevClientUrl spec = "http://localhost:" ++ show defaultClientPort ++ SP.fromAbsDirP (getBaseDir spec)
