module Wasp.Generator.WebAppGenerator.RunConfig
  ( makeEnvVars,
  )
where

import Wasp.Env (EnvVar)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import Wasp.Project.RunConfig (ProjectRunConfig (..))

-- | Managed environment variables. The app also reads its own dotenv files and
-- inherited environment on dev: https://github.com/wasp-lang/wasp/issues/4739.
makeEnvVars :: ProjectRunConfig -> [EnvVar]
makeEnvVars config =
  [ (Common.serverUrlEnvVarName, show config.serverUrl),
    (Common.clientPortEnvVarName, show config.clientPort)
  ]
