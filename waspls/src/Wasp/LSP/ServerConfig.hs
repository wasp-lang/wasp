module Wasp.LSP.ServerConfig
  ( ServerConfig (..)
  ) where

import Data.Default (Default(def))
import Data.Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)

data ServerConfig = ServerConfig {}

instance Default ServerConfig where
  def = ServerConfig {}

instance FromJSON ServerConfig where
  parseJSON (Object _) = pure ServerConfig
  parseJSON invalid =
    prependFailure
      "parsing ServerConfig failed, "
      (typeMismatch "Object" invalid)
