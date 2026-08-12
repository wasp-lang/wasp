module Wasp.Generator.SdkGenerator.WebSocketGenerator
  ( genWebSockets,
    webSocketPathInApp,
  )
where

import Data.Aeson (object, (.=))
import Data.Char (toLower)
import StrongPath (relfile)
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
  ( genFileCopy,
    mkTmplFdWithData,
  )
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Generator.WebSocket as AS.WS

genWebSockets :: AppSpec -> Generator [FileDraft]
genWebSockets spec
  | AS.WS.areWebSocketsUsed spec =
      sequence
        [ genServerWebSocketIndex spec,
          genFileCopy [relfile|client/webSocket/index.ts|],
          genFileCopy [relfile|client/webSocket/WebSocketProvider.tsx|],
          genClientWebSocket spec
        ]
  | otherwise = return []

genServerWebSocketIndex :: AppSpec -> Generator FileDraft
genServerWebSocketIndex spec =
  return $ mkTmplFdWithData [relfile|server/webSocket/index.ts|] tmplData
  where
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabled spec,
          "allEntities" .= map (makeJsonWithEntityData . fst) (AS.getEntities spec)
        ]

genClientWebSocket :: AppSpec -> Generator FileDraft
genClientWebSocket spec =
  return $ mkTmplFdWithData [relfile|client/webSocket/socket.ts|] tmplData
  where
    tmplData =
      object
        [ "autoConnect" .= map toLower (show shouldAutoConnect),
          "webSocketPath" .= webSocketPathInApp spec
        ]
    shouldAutoConnect = (AS.App.WS.autoConnect <$> maybeWebSocket) /= Just (Just False)
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec

-- | The path the app serves its websocket from, as the browser sees it. Nitro
-- serves the whole app (its routes included) from the app's base directory.
webSocketPathInApp :: AppSpec -> String
webSocketPathInApp spec = SP.fromAbsDirP (WebApp.getBaseDir spec) ++ drop 1 Server.webSocketRoutePath
