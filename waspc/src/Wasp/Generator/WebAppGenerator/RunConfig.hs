module Wasp.Generator.WebAppGenerator.RunConfig
  ( WebAppRunConfig (..),
    makeWebAppRunConfig,
  )
where

import Network.Socket (PortNumber)
import Wasp.Env (EnvVar)
import Wasp.Generator.RunConfig (HasEnvVars (..))
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Util.AppLocation as AL

data WebAppRunConfig = WebAppRunConfig
  { port :: PortNumber,
    envVars' :: [EnvVar]
  }
  deriving (Show, Eq)

instance HasEnvVars WebAppRunConfig where
  envVars = envVars'
  replaceEnvVars config newEnvVars = config {envVars' = newEnvVars}

makeWebAppRunConfig :: AL.AppLocation -> String -> WebAppRunConfig
makeWebAppRunConfig location serverUrl =
  WebAppRunConfig
    location.port
    [ (Common.serverUrlEnvVarName, serverUrl)
    ]
