module Wasp.Project.WebApp.Common
  ( getBaseDir,
    getDefaultDevClientUrl,
    defaultClientPort,
    serverUrlEnvVarName,
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

defaultClientPort :: Int
defaultClientPort = 3000

getDefaultDevClientUrl :: AppSpec -> String
getDefaultDevClientUrl spec = "http://localhost:" ++ show defaultClientPort ++ SP.fromAbsDirP (getBaseDir spec)
