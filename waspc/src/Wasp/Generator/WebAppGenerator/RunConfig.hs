module Wasp.Generator.WebAppGenerator.RunConfig
  ( ClientRunConfig (..),
    makeClientRunConfig,
  )
where

import Network.Socket (PortNumber)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator.WebAppGenerator.Common as Common
import qualified Wasp.Util.AppLocation as AL

data ClientRunConfig = ClientRunConfig
  { port :: PortNumber,
    envVars :: [EnvVar]
  }
  deriving (Show, Eq)

makeClientRunConfig :: AL.AppLocation -> String -> ClientRunConfig
makeClientRunConfig location serverUrl =
  ClientRunConfig
    location.port
    [ (Common.serverUrlEnvVarName, serverUrl)
    ]
