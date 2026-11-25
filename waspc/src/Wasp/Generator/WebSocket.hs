module Wasp.Generator.WebSocket
  ( areWebSocketsUsed,
    webSocketsNpmDeps,
  )
where

import Data.Maybe (isJust)
import Wasp.AppSpec (AppSpec (..))
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.Valid (getApp)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.OptionalFeatureNpmDeps.Common (OptionalFeatureNpmDeps (..))
import qualified Wasp.SemanticVersion as SV

areWebSocketsUsed :: AppSpec -> Bool
areWebSocketsUsed spec = isJust $ AS.App.webSocket $ getApp spec

webSocketsNpmDeps :: OptionalFeatureNpmDeps
webSocketsNpmDeps =
  OptionalFeatureNpmDeps
    { featureDescription = "WebSockets",
      isFeatureUsed = areWebSocketsUsed,
      dependencies =
        [ Npm.Dependency.make ("socket.io", show socketIoVersionRange),
          Npm.Dependency.make ("socket.io-client", show socketIoVersionRange),
          Npm.Dependency.make ("@socket.io/component-emitter", show [SV.r|^4.0.0|])
        ],
      devDependencies = []
    }

socketIoVersionRange :: SV.Range
socketIoVersionRange = [SV.r|^4.6.1|]
