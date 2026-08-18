module Wasp.Generator.ServerGenerator.RunConfig
  ( ServerRunConfig (..),
    makeServerRunConfig,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl, port, url)
import Wasp.Env (EnvVar, HasEnvVars (..))
import qualified Wasp.Generator.ServerGenerator.Common as Common

newtype ServerRunConfig = ServerRunConfig
  { -- These might not hold all the environment variables that the server uses,
    -- as it also reads from `.env.server` files and the current environment,
    -- autonomously. This holds the necessary environment variables for the app
    -- components to know where to run and where to communicate with web app.
    -- https://github.com/wasp-lang/wasp/issues/4739
    envVars :: [EnvVar]
  }
  deriving (Show, Eq)

instance HasEnvVars ServerRunConfig where
  getEnvVars = envVars
  setEnvVars config newEnvVars = config {envVars = newEnvVars}

makeServerRunConfig :: AppComponentUrl -> String -> ServerRunConfig
makeServerRunConfig expectedUrl clientUrl =
  ServerRunConfig
    [ (Common.clientUrlEnvVarName, clientUrl),
      (Common.serverUrlEnvVarName, url expectedUrl),
      (Common.serverPortEnvVarName, show $ port expectedUrl)
    ]
