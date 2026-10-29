module Wasp.Generator.ServerGenerator.RunConfig
  ( makeEnvVars,
  )
where

import Network.URI (uriToString)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Project.RunConfig (ProjectRunConfig (..))

-- | The env vars that tell the server where to run and how to reach the other
-- components. The server also reads its own dotenv files and the inherited
-- environment: https://github.com/wasp-lang/wasp/issues/4739.
makeEnvVars :: ProjectRunConfig -> [EnvVar]
makeEnvVars config =
  [ (Common.clientUrlEnvVarName, uriToString id config.clientUrl ""),
    (Common.serverUrlEnvVarName, uriToString id config.serverUrl ""),
    (Common.serverPortEnvVarName, show config.serverPort)
  ]
