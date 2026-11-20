module Wasp.Generator.WebAppGenerator.RunConfig
  ( makeEnvVars,
  )
where

import Wasp.Env (EnvVar)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import Wasp.Project.Db.Dev (devDatabaseEnvVars)
import Wasp.Project.RunConfig (ProjectRunConfig (..))
import Wasp.Util.Network.URI (showUri)

-- | The env vars that tell the web app where to run and how to reach the other
-- components. The web app also reads its own dotenv files and the inherited
-- environment: https://github.com/wasp-lang/wasp/issues/4739.
--
-- The dev database URL is for Prisma Studio, which the Vite dev server starts
-- as a Vite DevTools panel. Vite doesn't expose it to the client code, since it
-- lacks the `REACT_APP_` prefix.
makeEnvVars :: ProjectRunConfig -> [EnvVar]
makeEnvVars config =
  [ (Common.serverUrlEnvVarName, showUri config.serverUrl),
    (Common.clientPortEnvVarName, show config.clientPort)
  ]
    ++ devDatabaseEnvVars config.databaseUrl
