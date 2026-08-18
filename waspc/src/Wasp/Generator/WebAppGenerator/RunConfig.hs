module Wasp.Generator.WebAppGenerator.RunConfig
  ( WebAppRunConfig (..),
    makeWebAppRunConfig,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.Env (EnvVar, HasEnvVars (..))
import qualified Wasp.Generator.WebAppGenerator.Common as Common

newtype WebAppRunConfig = WebAppRunConfig
  { -- These might not hold all the environment variables that the web app uses,
    -- as it also reads from `.env.client` files and the current environment,
    -- autonomously. This holds the necessary environment variables for the app
    -- components to know where to run and where to communicate with server.
    envVars :: [EnvVar]
  }
  deriving (Show, Eq)

instance HasEnvVars WebAppRunConfig where
  getEnvVars = envVars
  setEnvVars config newEnvVars = config {envVars = newEnvVars}

makeWebAppRunConfig :: AppComponentUrl -> String -> WebAppRunConfig
makeWebAppRunConfig expectedUrl serverUrl =
  WebAppRunConfig
    [ (Common.serverUrlEnvVarName, serverUrl),
      (Common.clientPortEnvVarName, show expectedUrl.port)
    ]
