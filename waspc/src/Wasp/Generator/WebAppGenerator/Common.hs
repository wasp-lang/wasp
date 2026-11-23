module Wasp.Generator.WebAppGenerator.Common
  ( getBaseDir,
    serverUrlEnvVarName,
    clientPortEnvVarName,
    waspExePathEnvVarName,
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
    maybeBaseDir = SP.parseAbsDirP =<< (AS.App.Client.baseDir =<< AS.App.client (getApp spec))

serverUrlEnvVarName :: String
serverUrlEnvVarName = "REACT_APP_API_URL"

clientPortEnvVarName :: String
clientPortEnvVarName =
  -- Not prefixed with `WASP_` because many deployment platforms use this env.
  "PORT"

-- | The path to the Wasp CLI executable that started the dev server. The Vite
-- DevTools panels use it to run Wasp commands with the same Wasp version.
waspExePathEnvVarName :: String
waspExePathEnvVarName = "WASP_EXE_PATH"
