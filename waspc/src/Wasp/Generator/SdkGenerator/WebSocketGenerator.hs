module Wasp.Generator.SdkGenerator.WebSocketGenerator
  ( genWebSockets,
    depsRequiredByWebSockets,
  )
where

import Data.Aeson (object, (.=))
import Data.Char (toLower)
import StrongPath (relfile)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import qualified Wasp.Generator.WebSocket as AS.WS

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
    genFileCopy = return . makeSdkProjectTmplFd SdkUserCoreProject

genWebSocketServerIndex :: AppSpec -> Generator FileDraft
genWebSocketServerIndex spec = return $ makeSdkProjectTmplFdWithData SdkUserCoreProject [relfile|server/webSocket/index.ts|] tmplData
  where
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabled spec,
          "userWebSocketFn" .= extImportToImportJson mayebWebSocketFn,
          "allEntities" .= map (makeJsonWithEntityData . fst) (AS.getEntities spec)
        ]
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec
    mayebWebSocketFn = AS.App.WS.fn <$> maybeWebSocket

genWebSocketProvider :: AppSpec -> Generator FileDraft
genWebSocketProvider spec = return $ makeSdkProjectTmplFdWithData SdkUserCoreProject [relfile|client/webSocket/WebSocketProvider.tsx|] tmplData
  where
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec
    shouldAutoConnect = (AS.App.WS.autoConnect <$> maybeWebSocket) /= Just (Just False)
    tmplData = object ["autoConnect" .= map toLower (show shouldAutoConnect)]

depsRequiredByWebSockets :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByWebSockets spec =
  if AS.WS.areWebSocketsUsed spec
    then AS.WS.sdkDepsRequiredForWebSockets
    else []
