module Wasp.Generator.WebSocket
  ( areWebSocketsUsed,
  )
where

import Data.Maybe (isJust)
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.Valid (getApp)

areWebSocketsUsed :: AppSpec -> Bool
areWebSocketsUsed spec = isJust $ AS.App.webSocket $ snd $ getApp spec
