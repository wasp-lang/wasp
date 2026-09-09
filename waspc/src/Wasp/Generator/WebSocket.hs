module Wasp.Generator.WebSocket
  ( areWebSocketsUsed,
    serverDepsRequiredForWebSockets,
    sdkDepsRequiredForWebSockets,
  )
where

import Wasp.AppSpec.Valid (areWebSocketsUsed)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.SemanticVersion as SV

socketIoVersionRange :: SV.Range
socketIoVersionRange = [SV.r|^4.6.1|]

socketIoComponentEmitterVersionRange :: SV.Range
socketIoComponentEmitterVersionRange = [SV.r|^4.0.0|]

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
