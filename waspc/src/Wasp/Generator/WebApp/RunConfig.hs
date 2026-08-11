module Wasp.Generator.WebApp.RunConfig
  ( ClientRunConfig (..),
    makeDefault,
    url,
    devEnvVars,
  )
where

import Data.Maybe (maybeToList)
import Network.Socket (PortNumber)
import StrongPath (Abs, Dir, Path, Posix)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.WebAppGenerator.Common as Common

-- | Configuration for a run of the app's client (web app): where it runs and
-- what it knows about its surroundings.
data ClientRunConfig = ClientRunConfig
  { port :: PortNumber,
    baseDir :: Path Posix Abs (Dir ()),
    -- | URL of the server backing this client. 'Nothing' until the client is
    -- wired to a server (see 'Wasp.Cli.AppComponents.makeDevRunConfigs').
    serverUrl :: Maybe String
  }
  deriving (Show, Eq)

-- | Takes the 'AppSpec' because the client may be served under a custom base
-- dir, which is part of its URL. For now, the client always runs on the
-- default port. Making it configurable comes later.
makeDefault :: AppSpec -> ClientRunConfig
makeDefault spec =
  ClientRunConfig
    { port = fromIntegral Common.defaultClientPort,
      baseDir = Common.getBaseDir spec,
      serverUrl = Nothing
    }

url :: ClientRunConfig -> String
url config = "http://localhost:" ++ show config.port ++ SP.fromAbsDirP config.baseDir

-- | The env vars Wasp sets for the client build and dev processes. The server
-- URL is omitted while the client isn't wired to a server.
devEnvVars :: ClientRunConfig -> [EnvVar]
devEnvVars config =
  [(Common.serverUrlEnvVarName, serverUrl) | serverUrl <- maybeToList config.serverUrl]
