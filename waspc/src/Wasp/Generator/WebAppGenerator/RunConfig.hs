module Wasp.Generator.WebAppGenerator.RunConfig
  ( ClientRunConfig (..),
    makeClientRunConfig,
  )
where

import Network.Socket (PortNumber)
import Wasp.Env (EnvVar, EnvVarName, overrideEnvVars)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Util.AppLocation as AL

data ClientRunConfig = ClientRunConfig
  { port :: PortNumber,
    url :: String,
    envVars :: [EnvVar]
  }
  deriving (Show, Eq)

makeClientRunConfig ::
  AL.AppLocation ->
  String ->
  [EnvVar] ->
  Either [EnvVarName] ClientRunConfig
makeClientRunConfig location serverUrl extraEnvVars =
  ClientRunConfig
    location.port
    (AL.url location)
    <$> overrideEnvVars requiredEnvVars extraEnvVars
  where
    requiredEnvVars =
      [ (Common.serverUrlEnvVarName, serverUrl)
      ]
