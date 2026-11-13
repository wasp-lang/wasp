module Wasp.Generator.ServerGenerator.RunConfig
  ( makeEnvVars,
  )
where

import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Project.Db.Dev (devDatabaseEnvVars)
import Wasp.Project.RunConfig (ProjectRunConfig (..))
import Wasp.Util.Network.URI (showUri)

-- | The env vars that tell the server where to run, how to reach the other
-- components, and which database to use. The server also reads its own dotenv
-- files and the inherited environment: https://github.com/wasp-lang/wasp/issues/4739.
makeEnvVars :: ProjectRunConfig -> [EnvVar]
makeEnvVars config =
  [ (Common.clientUrlEnvVarName, showUri config.clientUrl),
    (Common.serverUrlEnvVarName, showUri config.serverUrl),
    (Common.serverPortEnvVarName, show config.serverPort)
  ]
    ++ devDatabaseEnvVars config.databaseUrl
