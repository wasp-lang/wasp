module Wasp.Generator.SdkGenerator.WebSocketGenerator
  ( genWebSockets,
    depsRequiredByWebSockets,
  )
where

import Data.Aeson (object, (.=))
import Data.Char (toLower)
import Data.Maybe (fromJust)
import StrongPath (reldir, relfile)
import qualified StrongPath as SP
import StrongPath.Operations ((</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (extImportToJsImport)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.WebSocket as AS.WS
import qualified Wasp.Util.StrongPath as SP

genWebSockets :: AppSpec -> Generator [FileDraft]
genWebSockets spec
  | AS.WS.areWebSocketsUsed spec =
    sequence
      [ genWebSocketServerIndex spec,
        genFileCopy [relfile|client/webSocket/index.ts|],
        genWebSocketProvider spec
      ]
  | otherwise = return []
  where
    genFileCopy = return . C.mkTmplFd

genWebSocketServerIndex :: AppSpec -> Generator FileDraft
genWebSocketServerIndex spec = return $ C.mkTmplFdWithData relIndexTsPath tmplData
  where
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabled spec,
          "userWebSocketFn" .= GJI.jsImportToImportJson (extImportToJsImport sdkRootDirFromWebsocketsDir <$> mayebWebSocketFn),
          "allEntities" .= map (makeJsonWithEntityData . fst) (AS.getEntities spec)
        ]
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec
    mayebWebSocketFn = AS.App.WS.fn <$> maybeWebSocket
    relServerWebsocketsDirPath = [reldir|server/webSocket|]
    relIndexTsPath = relServerWebsocketsDirPath </> [relfile|index.ts|]
    sdkRootDirFromWebsocketsDir = SP.reversePosixPath $ fromJust $ SP.relDirToPosix relServerWebsocketsDirPath

genWebSocketProvider :: AppSpec -> Generator FileDraft
genWebSocketProvider spec = return $ C.mkTmplFdWithData [relfile|client/webSocket/WebSocketProvider.tsx|] tmplData
  where
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec
    shouldAutoConnect = (AS.App.WS.autoConnect <$> maybeWebSocket) /= Just (Just False)
    tmplData = object ["autoConnect" .= map toLower (show shouldAutoConnect)]

depsRequiredByWebSockets :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByWebSockets spec
  | AS.WS.areWebSocketsUsed spec = AS.WS.sdkDepsRequiredForWebSockets
  | otherwise = AS.Dependency.fromList [("uuid", "^9.0.0")]
