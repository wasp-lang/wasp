module Wasp.Generator.SdkGenerator.UserCore.WebSocketGenerator
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
import Wasp.Generator.SdkGenerator.JsImport (extImportToImportJson)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFd,
    mkTmplFdWithData,
  )
import qualified Wasp.Generator.WebSocket as AS.WS

genWebSockets :: AppSpec -> Generator [FileDraft]
genWebSockets spec
  | AS.WS.areWebSocketsUsed spec =
      sequence
        [ genServerIndex spec,
          genClientIndex,
          genClientWebSocketProvider spec
        ]
  | otherwise = return []

genServerIndex :: AppSpec -> Generator FileDraft
genServerIndex spec =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|server/webSocket/index.ts|]
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabled spec,
          "userWebSocketFn" .= extImportToImportJson mayebWebSocketFn,
          "allEntities" .= map (makeJsonWithEntityData . fst) (AS.getEntities spec)
        ]
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec
    mayebWebSocketFn = AS.App.WS.fn <$> maybeWebSocket

genClientIndex :: Generator FileDraft
genClientIndex =
  return $ mkTmplFd tempFile
  where
    tempFile = [relfile|client/webSocket/index.ts|]

genClientWebSocketProvider :: AppSpec -> Generator FileDraft
genClientWebSocketProvider spec =
  return $ mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|client/webSocket/WebSocketProvider.tsx|]
    tmplData = object ["autoConnect" .= map toLower (show shouldAutoConnect)]
    shouldAutoConnect = (AS.App.WS.autoConnect <$> maybeWebSocket) /= Just (Just False)
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec

depsRequiredByWebSockets :: AppSpec -> [Npm.Dependency.Dependency]
depsRequiredByWebSockets spec =
  if AS.WS.areWebSocketsUsed spec
    then AS.WS.sdkDepsRequiredForWebSockets
    else []
