module Wasp.Generator.ServerGenerator.RunConfig
  ( ServerRunConfig (..),
    makeServerRunConfig,
  )
where

import Wasp.Env (EnvVar)
import Wasp.Generator.AppComponentUrl (AppComponentUrl, url)
import Wasp.Generator.RunConfig (HasEnvVars (..))
import qualified Wasp.Generator.ServerGenerator.Common as Common

newtype ServerRunConfig = ServerRunConfig
  { envVars' :: [EnvVar]
  }
  deriving (Show, Eq)

instance HasEnvVars ServerRunConfig where
  envVars = envVars'
  replaceEnvVars config newEnvVars = config {envVars' = newEnvVars}

makeServerRunConfig :: AppComponentUrl -> String -> ServerRunConfig
makeServerRunConfig expectedUrl clientUrl =
  ServerRunConfig
    [ (Common.clientUrlEnvVarName, clientUrl),
      (Common.serverUrlEnvVarName, url expectedUrl)
    ]
