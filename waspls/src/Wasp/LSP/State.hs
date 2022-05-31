module Wasp.LSP.State
  ( HandlerM,
    Severity (..),
    State,
    ServerConfig,
  )
where

import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Default (Default (def))
import Data.Text (Text)
import Language.LSP.Server (LspT)

type HandlerM =
  ExceptT (Severity, Text) (StateT State (LspT ServerConfig IO))

-- | Log levels
data Severity
  = -- | Displayed to user as an error
    Error
  | -- | Displayed to user as a warning
    Warning
  | -- | Displayed to user
    Info
  | -- | Not displayed to the user
    Log

data ServerConfig = ServerConfig {}

instance Default ServerConfig where
  def = ServerConfig {}

instance FromJSON ServerConfig where
  parseJSON (Object _) = pure ServerConfig
  parseJSON invalid =
    prependFailure
      "parsing ServerConfig failed, "
      (typeMismatch "Object" invalid)

data State = State {}

instance Default State where
  def = State {}
