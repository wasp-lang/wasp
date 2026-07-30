module Wasp.Generator.WebAppGenerator.Common
  ( getBaseDir,
    getDevClientUrl,
    clientDevPortEnvVarName,
    serverUrlEnvVarName,
    getDevClientEnvVars,
  )
where

import Data.Maybe (fromMaybe)
import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path, Posix, absdirP)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.Valid (getApp)
import Wasp.Env (EnvVar)
import Wasp.Project.Apps (Apps (..))

getBaseDir :: AppSpec -> Path Posix Abs (Dir ())
getBaseDir spec = fromMaybe [absdirP|/|] maybeBaseDir
  where
    maybeBaseDir = SP.parseAbsDirP =<< (AS.App.Client.baseDir =<< AS.App.client (snd $ getApp spec))

serverUrlEnvVarName :: String
serverUrlEnvVarName = "REACT_APP_API_URL"

clientDevPortEnvVarName :: String
clientDevPortEnvVarName = "PORT"

getDevClientUrl :: AppSpec -> PortNumber -> String
getDevClientUrl spec port = "http://localhost:" ++ show port ++ SP.fromAbsDirP (getBaseDir spec)

getDevClientEnvVars :: Apps (PortNumber, String) -> [EnvVar]
getDevClientEnvVars (Apps {client = (clientPort, _), server = (_, serverUrl)}) =
  [ (clientDevPortEnvVarName, show clientPort),
    (serverUrlEnvVarName, serverUrl)
  ]
