module Wasp.Generator.ServerGenerator.RunConfig
  ( ServerRunConfig (..),
    makeServerRunConfig,
  )
where

import Network.Socket (PortNumber)
import Wasp.Env (EnvVar, EnvVarName, overrideEnvVars)
import qualified Wasp.Generator.ServerGenerator.Common as Common
import qualified Wasp.Util.AppLocation as AL

data ServerRunConfig = ServerRunConfig
  { port :: PortNumber,
    url :: String,
    envVars :: [EnvVar]
  }
  deriving (Show, Eq)

makeServerRunConfig ::
  AL.AppLocation ->
  String ->
  [EnvVar] ->
  Either [EnvVarName] ServerRunConfig
makeServerRunConfig location clientUrl extraEnvVars =
  ServerRunConfig
    location.port
    (AL.url location)
    <$> overrideEnvVars requiredEnvVars extraEnvVars
  where
    requiredEnvVars =
      [ (Common.clientUrlEnvVarName, clientUrl),
        (Common.serverUrlEnvVarName, AL.url location)
      ]
