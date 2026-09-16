module Wasp.Generator.ServerGenerator.RunConfig
  ( ServerRunConfig (..),
    makeServerRunConfig,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl)
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.Env (EnvVar, HasEnvVars (..))
import qualified Wasp.Generator.ServerGenerator.Common as Common

data ServerRunConfig = ServerRunConfig
  { url :: AppComponentUrl,
    -- These might not hold all the environment variables that the server uses,
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
    expectedUrl
    [ (Common.clientUrlEnvVarName, clientUrl),
      (Common.serverUrlEnvVarName, AppComponentUrl.url expectedUrl),
      (Common.serverPortEnvVarName, show $ AppComponentUrl.port expectedUrl)
    ]
