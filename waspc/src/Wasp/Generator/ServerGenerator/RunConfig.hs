module Wasp.Generator.ServerGenerator.RunConfig
  ( ServerRunConfig (..),
    makeServerRunConfig,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl, url)
import Wasp.Env (EnvVar, HasEnvVars (..))
import qualified Wasp.Generator.ServerGenerator.Common as Common

newtype ServerRunConfig = ServerRunConfig
  { envVars' :: [EnvVar]
  }
  deriving (Show, Eq)

instance HasEnvVars ServerRunConfig where
  envVars = envVars'
  setEnvVars config newEnvVars = config {envVars' = newEnvVars}

makeServerRunConfig :: AppComponentUrl -> String -> ServerRunConfig
makeServerRunConfig expectedUrl clientUrl =
  ServerRunConfig
    [ (Common.clientUrlEnvVarName, clientUrl),
      (Common.serverUrlEnvVarName, url expectedUrl)
    ]
