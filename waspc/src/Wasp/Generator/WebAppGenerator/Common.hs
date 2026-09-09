module Wasp.Generator.WebAppGenerator.Common
  ( getBaseDir,
    serverUrlEnvVarName,
    devProxyTargetEnvVarName,
    clientPortEnvVarName,
  )
where

import Data.Maybe (fromMaybe)
import StrongPath (Abs, Dir, Path, Posix, absdirP)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)

getBaseDir :: AppSpec -> Path Posix Abs (Dir ())
getBaseDir spec = fromMaybe [absdirP|/|] maybeBaseDir
  where
    maybeBaseDir = SP.parseAbsDirP =<< (AS.App.Client.baseDir =<< AS.App.client (snd $ getApp spec))

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
