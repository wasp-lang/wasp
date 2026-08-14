module Wasp.Generator.WebAppGenerator.RunConfig
  ( WebAppRunConfig (..),
    makeWebAppRunConfig,
  )
where

import Network.Socket (PortNumber)
import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.Env (EnvVar, HasEnvVars (..))
import qualified Wasp.Generator.WebAppGenerator.Common as Common

data WebAppRunConfig = WebAppRunConfig
  { port :: PortNumber,
    envVars' :: [EnvVar]
  }
  deriving (Show, Eq)

instance HasEnvVars WebAppRunConfig where
  envVars = envVars'
  setEnvVars config newEnvVars = config {envVars' = newEnvVars}

makeWebAppRunConfig :: AppComponentUrl -> String -> WebAppRunConfig
makeWebAppRunConfig expectedUrl serverUrl =
  WebAppRunConfig
    expectedUrl.port
    [ (Common.serverUrlEnvVarName, serverUrl)
    ]
