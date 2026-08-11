module Wasp.Generator.ServerGenerator.RunConfig
  ( ServerRunConfig (..),
    makeServerRunConfig,
  )
where

import Wasp.Env (EnvVar, EnvVarName, overrideEnvVars)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Util.AppLocation as AL

newtype ServerRunConfig = ServerRunConfig
  { envVars :: [EnvVar]
  }
  deriving (Show, Eq)

makeServerRunConfig ::
  AL.AppLocation ->
  String ->
  [EnvVar] ->
  Either [EnvVarName] ServerRunConfig
makeServerRunConfig location clientUrl extraEnvVars =
  ServerRunConfig
    <$> overrideEnvVars requiredEnvVars extraEnvVars
  where
    requiredEnvVars =
      [ (Common.clientUrlEnvVarName, clientUrl),
        (Common.serverUrlEnvVarName, AL.url location)
      ]
