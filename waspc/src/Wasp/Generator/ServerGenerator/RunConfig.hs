module Wasp.Generator.ServerGenerator.RunConfig
  ( ServerRunConfig (..),
    makeServerRunConfig,
  )
where

import Wasp.Env (EnvVar)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Util.AppLocation as AL

newtype ServerRunConfig = ServerRunConfig
  { envVars :: [EnvVar]
  }
  deriving (Show, Eq)

makeServerRunConfig :: AL.AppLocation -> String -> ServerRunConfig
makeServerRunConfig location clientUrl =
  ServerRunConfig
    [ (Common.serverPortEnvVarName, show $ AL.port location),
      (Common.clientUrlEnvVarName, clientUrl),
      (Common.serverUrlEnvVarName, AL.url location)
    ]
