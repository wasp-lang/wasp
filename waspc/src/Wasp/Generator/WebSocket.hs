module Wasp.Generator.WebSocket
  ( areWebSocketsUsed,
    serverDepsRequiredForWebSockets,
    clientDepsRequiredForWebSockets,
  )
where

import Data.Maybe (isJust)
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.SemanticVersion as SV

areWebSocketsUsed :: AppSpec -> Bool
areWebSocketsUsed spec = isJust $ AS.App.webSocket $ snd $ getApp spec

socketIoVersionRange :: SV.Range
socketIoVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 4 6 1)]

socketIoComponentEmitterVersionRange :: SV.Range
socketIoComponentEmitterVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.Version 4 0 0)]

serverDepsRequiredForWebSockets :: [AS.Dependency.Dependency]
serverDepsRequiredForWebSockets =
  [ AS.Dependency.make ("socket.io", show socketIoVersionRange),
    AS.Dependency.make ("@socket.io/component-emitter", show socketIoComponentEmitterVersionRange)
  ]

clientDepsRequiredForWebSockets :: [AS.Dependency.Dependency]
clientDepsRequiredForWebSockets =
  [ AS.Dependency.make ("socket.io-client", show socketIoVersionRange),
    AS.Dependency.make ("@socket.io/component-emitter", show socketIoComponentEmitterVersionRange)
  ]
