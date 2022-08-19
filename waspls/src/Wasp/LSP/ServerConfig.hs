module Wasp.LSP.ServerConfig
  ( ServerConfig (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), Value (Object))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Default (Default (def))

data ServerConfig = ServerConfig {}

instance Default ServerConfig where
  def = ServerConfig {}

instance FromJSON ServerConfig where
  parseJSON (Object _) = pure ServerConfig
  parseJSON invalid =
    prependFailure
      "parsing ServerConfig failed, "
      (typeMismatch "Object" invalid)
