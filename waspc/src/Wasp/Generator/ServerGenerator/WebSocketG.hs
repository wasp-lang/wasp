module Wasp.Generator.ServerGenerator.WebSocketG
  ( depsRequiredByWebSockets,
    genWebSockets,
    genWebSocketTs,
    mkWebSocketFnImport,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import StrongPath
  ( Dir,
    Path,
    Posix,
    Rel,
    reldirP,
    relfile,
  )
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.App.WebSocket (WebSocket)
import qualified Wasp.AppSpec.App.WebSocket as AS.App.WS
import Wasp.AppSpec.Valid (getApp, isAuthEnabled)
import Wasp.Generator.Common
  ( makeJsonWithEntityData,
  )
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import Wasp.Generator.ServerGenerator.JsImport (extImportToImportJson)
import qualified Wasp.Generator.WebSocket as AS.WS

depsRequiredByWebSockets :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByWebSockets spec
  | AS.WS.areWebSocketsUsed spec = AS.WS.serverDepsRequiredForWebSockets
  | otherwise = []

genWebSockets :: AppSpec -> Generator [FileDraft]
genWebSockets spec
  | AS.WS.areWebSocketsUsed spec =
      sequence
        [ genWebSocketTs spec
        ]
  | otherwise = return []

genWebSocketTs :: AppSpec -> Generator FileDraft
genWebSocketTs spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/webSocket.ts|])
      (C.asServerFile [relfile|src/webSocket.ts|])
      ( Just $
          object
            [ "isAuthEnabled" .= isAuthEnabled spec,
              "userWebSocketFn" .= mkWebSocketFnImport maybeWebSocket,
              "entities" .= maybe [] (map (makeJsonWithEntityData . AS.refName)) (AS.App.WS.entities =<< maybeWebSocket)
            ]
      )
  where
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec

mkWebSocketFnImport :: Maybe WebSocket -> Aeson.Value
mkWebSocketFnImport maybeWebSocket =
  let maybeWebSocketFn = AS.App.WS.fn <$> maybeWebSocket
   in extImportToImportJson relPathToServerSrcDir maybeWebSocketFn
  where
    relPathToServerSrcDir :: Path Posix (Rel importLocation) (Dir C.ServerSrcDir)
    relPathToServerSrcDir = [reldirP|./|]
