module Wasp.Generator.WebAppGenerator.Common
  ( getBaseDir,
    clientDevPortEnvVarName,
    defaultDevClientPort,
    makeDevClientLocation,
    makeDefaultDevClientLocation,
    serverUrlEnvVarName,
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
import Wasp.Util.AppLocation (AppLocation (..))

getBaseDir :: AppSpec -> Path Posix Abs (Dir ())
getBaseDir spec = fromMaybe [absdirP|/|] maybeBaseDir
  where
    maybeBaseDir = SP.parseAbsDirP =<< (AS.App.Client.baseDir =<< AS.App.client (snd $ getApp spec))

serverUrlEnvVarName :: String
serverUrlEnvVarName = "REACT_APP_API_URL"

clientDevPortEnvVarName :: String
clientDevPortEnvVarName = "PORT"

defaultDevClientPort :: PortNumber
defaultDevClientPort = 3000

-- | Where the client runs in development, on the given port.
makeDevClientLocation :: AppSpec -> PortNumber -> AppLocation
makeDevClientLocation spec devClientPort =
  Local {port = devClientPort, baseDir = Just $ getBaseDir spec}

-- | Where the client runs in development when the user doesn't choose a port.
-- Processes that never bind a port (like the test runner) also use it as a
-- placeholder.
makeDefaultDevClientLocation :: AppSpec -> AppLocation
makeDefaultDevClientLocation spec = makeDevClientLocation spec defaultDevClientPort
