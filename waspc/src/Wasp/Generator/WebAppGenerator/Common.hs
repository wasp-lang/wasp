module Wasp.Generator.WebAppGenerator.Common
  ( getBaseDir,
    makeDefaultDevClientUrl,
    serverUrlEnvVarName,
  )
where

import Data.Maybe (fromMaybe)
import StrongPath (Abs, Dir, Path, Posix, absdirP)
import qualified StrongPath as SP
import Wasp.AppComponentUrl (AppComponentUrl (..))
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

makeDefaultDevClientUrl :: AppSpec -> AppComponentUrl
makeDefaultDevClientUrl spec =
  Local {port = 3000, path = Just $ getBaseDir spec}
