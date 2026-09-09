module Wasp.Generator.WebAppGenerator.RunConfig
  ( WebAppRunConfig (..),
    makeWebAppRunConfig,
  )
where

import Wasp.AppComponentUrl (AppComponentUrl)
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec.App.Deployment (DeploymentMode (..))
import Wasp.Env (EnvVar, HasEnvVars (..))
import qualified Wasp.Generator.WebAppGenerator.Common as Common

data WebAppRunConfig = WebAppRunConfig
  { url :: AppComponentUrl,
    -- These might not hold all the environment variables that the web app uses,
    -- as it also reads from `.env.client` files and the current environment,
    -- autonomously. This holds the necessary environment variables for the app
    -- components to know where to run and where to communicate with server.
    -- https://github.com/wasp-lang/wasp/issues/4739
    envVars :: [EnvVar]
  }
  deriving (Show, Eq)

instance HasEnvVars WebAppRunConfig where
  getEnvVars = envVars
  setEnvVars config newEnvVars = config {envVars = newEnvVars}

makeWebAppRunConfig :: DeploymentMode -> AppComponentUrl -> AppComponentUrl -> WebAppRunConfig
makeWebAppRunConfig deploymentMode expectedUrl serverUrl =
  WebAppRunConfig
    expectedUrl
    [ serverUrlEnvVar,
      (Common.clientPortEnvVarName, show $ AppComponentUrl.port expectedUrl)
    ]
  where
    serverUrlEnvVar = case deploymentMode of
      -- The client calls the server on its own origin and the Vite dev server
      -- proxies those calls to the server.
      Single -> (Common.devProxyTargetEnvVarName, AppComponentUrl.origin serverUrl)
      Split -> (Common.serverUrlEnvVarName, AppComponentUrl.url serverUrl)
