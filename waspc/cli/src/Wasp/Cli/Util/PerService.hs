module Wasp.Cli.Util.PerService where

import Data.Foldable (toList)
import Data.List (intercalate)
import Network.Socket (PortNumber)
import Wasp.AppSpec (AppSpec)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.PerService (PerService (..), names)
import Wasp.Util (toUpperFirst)

getDevUrlMakers :: AppSpec -> PerService (PortNumber -> String)
getDevUrlMakers spec =
  PerService
    { client = WebApp.getDevClientUrl spec,
      server = Server.makeDevServerUrl
    }

getWaspEnvVars :: AppSpec -> PerService PortNumber -> PerService [EnvVar]
getWaspEnvVars spec ports =
  PerService
    { client = WebApp.getDevClientEnvVars,
      server = Server.getDevServerEnvVars
    }
    <*> pure locations
  where
    locations = liftA2 (,) ports urls
    urls = getDevUrlMakers spec <*> ports

-- | Tells the user where each of the app's parts is running. Wasp picks the
-- ports itself, so users can't know them in advance.
makeAppUrlsMessage :: PerService String -> String
makeAppUrlsMessage urls = intercalate "\n" . toList $ makeUrlLine <$> names <*> urls
  where
    makeUrlLine name url = " ℹ " ++ toUpperFirst name ++ ": " ++ url
