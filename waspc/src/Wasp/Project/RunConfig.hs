module Wasp.Project.RunConfig
  ( ProjectRunConfig (..),
  )
where

import Network.Socket (PortNumber)
import Network.URI (URI)

-- | URLs used to reach each component and ports on which they listen.
-- These can differ when a component runs behind a proxy.
data ProjectRunConfig = ProjectRunConfig
  { clientUrl :: URI,
    serverUrl :: URI,
    clientPort :: PortNumber,
    serverPort :: PortNumber
  }
  deriving (Show, Eq)
