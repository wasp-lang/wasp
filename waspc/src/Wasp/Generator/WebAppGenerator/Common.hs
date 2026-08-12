module Wasp.Generator.WebAppGenerator.Common
  ( getBaseDir,
    getDefaultDevClientUrl,
    getDefaultDevApiUrl,
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

-- | The URL the app is served from in development. The app's server serves both
-- the app's pages and its API, so this is also where the API lives.
getDefaultDevClientUrl :: AppSpec -> String
getDefaultDevClientUrl spec = "http://localhost:" ++ show defaultClientPort ++ SP.fromAbsDirP (getBaseDir spec)

-- | Where the client looks for the app's API in development. The API is served
-- from the app's own origin, so this is a path, and an empty one for an app
-- served from the root.
getDefaultDevApiUrl :: AppSpec -> String
getDefaultDevApiUrl = dropTrailingSlash . SP.fromAbsDirP . getBaseDir
  where
    dropTrailingSlash = reverse . dropWhile (== '/') . reverse
