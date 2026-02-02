module Wasp.Generator.WebSocket
  ( areWebSocketsUsed,
    serverDepsRequiredForWebSockets,
    sdkDepsRequiredForWebSockets,
  )
where

import Data.Maybe (isJust)
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.SemanticVersion as SV

areWebSocketsUsed :: AppSpec -> Bool
areWebSocketsUsed spec = isJust $ AS.App.webSocket $ snd $ getApp spec

socketIoVersionRange :: SV.Range
socketIoVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.FullVersion 4 6 1)]

socketIoComponentEmitterVersionRange :: SV.Range
socketIoComponentEmitterVersionRange = SV.Range [SV.backwardsCompatibleWith (SV.FullVersion 4 0 0)]

serverDepsRequiredForWebSockets :: [Npm.Dependency.Dependency]
serverDepsRequiredForWebSockets =
  [ Npm.Dependency.make ("socket.io", show socketIoVersionRange),
    Npm.Dependency.make ("@socket.io/component-emitter", show socketIoComponentEmitterVersionRange)
  ]

sdkDepsRequiredForWebSockets :: [Npm.Dependency.Dependency]
sdkDepsRequiredForWebSockets =
  [ Npm.Dependency.make ("socket.io", show socketIoVersionRange),
    Npm.Dependency.make ("socket.io-client", show socketIoVersionRange),
    Npm.Dependency.make ("@socket.io/component-emitter", show socketIoComponentEmitterVersionRange)
  ]
