{-# LANGUAGE TupleSections #-}

module Wasp.Generator.ServerGenerator.RunConfig
  ( makeEnvVars,
  )
where

import Data.Maybe (maybeToList)
import Network.URI (uriToString)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import Wasp.Project.Db (databaseUrlEnvVarName)
import Wasp.Project.RunConfig (ProjectRunConfig (..))

-- | The env vars that tell the server where to run, how to reach the other
-- components, and which database to use. The server also reads its own dotenv
-- files and the inherited environment: https://github.com/wasp-lang/wasp/issues/4739.
makeEnvVars :: ProjectRunConfig -> [EnvVar]
makeEnvVars config =
  [ (Common.clientUrlEnvVarName, uriToString id config.clientUrl ""),
    (Common.serverUrlEnvVarName, uriToString id config.serverUrl ""),
    (Common.serverPortEnvVarName, show config.serverPort)
  ]
    ++ maybeToList ((databaseUrlEnvVarName,) <$> config.databaseUrl)
