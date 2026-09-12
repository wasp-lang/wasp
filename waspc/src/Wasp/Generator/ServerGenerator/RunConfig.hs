module Wasp.Generator.ServerGenerator.RunConfig
  ( ServerRunConfig (..),
    makeServerRunConfig,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl)
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec.App.Deployment (DeploymentMode (..))
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

makeServerRunConfig :: DeploymentMode -> AppComponentUrl -> AppComponentUrl -> ServerRunConfig
makeServerRunConfig deploymentMode expectedUrl clientUrl =
  ServerRunConfig
    expectedUrl
    [ (Common.clientUrlEnvVarName, AppComponentUrl.url clientUrl),
      (Common.serverUrlEnvVarName, publicServerUrl),
      (Common.serverPortEnvVarName, show $ AppComponentUrl.port expectedUrl)
    ]
  where
    publicServerUrl = case deploymentMode of
      -- The server is reached through the client's origin, in dev via the
      -- Vite proxy and in production because the server serves the client.
      -- TODO: production does not serve the client from the server yet, and
      -- the client URL will need the client base dir appended once it does.
      Single -> AppComponentUrl.origin clientUrl
      Split -> AppComponentUrl.url expectedUrl
