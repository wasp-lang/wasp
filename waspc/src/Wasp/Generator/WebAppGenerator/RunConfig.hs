module Wasp.Generator.WebAppGenerator.RunConfig
  ( WebAppRunConfig (..),
    makeWebAppRunConfig,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.Env (EnvVar, HasEnvVars (..))
import qualified Wasp.Generator.WebAppGenerator.Common as Common

newtype WebAppRunConfig = WebAppRunConfig
  { envVars :: [EnvVar]
  }
  deriving (Show, Eq)

instance HasEnvVars WebAppRunConfig where
  getEnvVars = envVars
  setEnvVars config newEnvVars = config {envVars = newEnvVars}

makeWebAppRunConfig :: AppComponentUrl -> String -> WebAppRunConfig
makeWebAppRunConfig _ serverUrl =
  WebAppRunConfig
    [ (Common.serverUrlEnvVarName, serverUrl)
    ]
