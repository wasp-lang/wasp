-- | Generates the Nitro route the app's websocket connections arrive on. It
-- wraps the user's websocket definition with the hooks Nitro's websocket
-- support (crossws) expects.
module Wasp.Generator.ServerGenerator.WebSocketG
  ( genWebSockets,
    webSocketRouteFileInServerRootDir,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (File', Path', Rel, reldirP, relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import qualified Wasp.Generator.WebSocket as AS.WS

genWebSockets :: AppSpec -> Generator [FileDraft]
genWebSockets spec
  | AS.WS.areWebSocketsUsed spec = sequence [genWebSocketRoute spec]
  | otherwise = return []

genWebSocketRoute :: AppSpec -> Generator FileDraft
genWebSocketRoute spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile webSocketRouteFileInServerRootDir)
      (C.asServerFile webSocketRouteFileInServerRootDir)
      (Just $ object ["userWebSocketFn" .= extImportToImportJson [reldirP|../|] maybeWebSocketFn])
  where
    maybeWebSocketFn = AS.App.WS.fn <$> AS.App.webSocket (snd $ getApp spec)

-- | The Nitro route handler for the app's websocket, which the app's Vite
-- config points Nitro at.
webSocketRouteFileInServerRootDir :: Path' (Rel C.ServerRootDir) File'
webSocketRouteFileInServerRootDir = [relfile|src/nitro/webSocket.ts|]
