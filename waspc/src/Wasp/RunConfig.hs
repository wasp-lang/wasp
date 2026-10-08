module Wasp.RunConfig (RunConfigs (..)) where

import Wasp.Db.RunConfig (DbRunConfig)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)

data RunConfigs = RunConfigs
  { client :: WebAppRunConfig,
    server :: ServerRunConfig,
    database :: DbRunConfig
  }
