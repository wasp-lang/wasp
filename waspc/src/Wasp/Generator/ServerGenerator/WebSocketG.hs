module Wasp.Generator.ServerGenerator.WebSocketG
  ( depsRequiredByWebSockets,
    genWebSockets,
    genWebSocketIndex,
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
        [ genWebSocketIndex spec,
          genWebSocketInitialization spec
        ]
  | otherwise = return []

genWebSocketIndex :: AppSpec -> Generator FileDraft
genWebSocketIndex spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/webSocket/index.ts|])
      (C.asServerFile [relfile|src/webSocket/index.ts|])
      ( Just $
          object
            [ "isAuthEnabled" .= isAuthEnabled spec,
              "userWebSocketFn" .= mkWebSocketFnImport maybeWebSocket [reldirP|../|],
              "allEntities" .= map (makeJsonWithEntityData . fst) (AS.getEntities spec)
            ]
      )
  where
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec

genWebSocketInitialization :: AppSpec -> Generator FileDraft
genWebSocketInitialization spec =
  return $
    C.mkTmplFdWithDstAndData
      (C.asTmplFile [relfile|src/webSocket/initialization.ts|])
      (C.asServerFile [relfile|src/webSocket/initialization.ts|])
      ( Just $
          object
            [ "isAuthEnabled" .= isAuthEnabled spec,
              "userWebSocketFn" .= mkWebSocketFnImport maybeWebSocket [reldirP|../|],
              "allEntities" .= map (makeJsonWithEntityData . fst) (AS.getEntities spec)
            ]
      )
  where
    maybeWebSocket = AS.App.webSocket $ snd $ getApp spec

mkWebSocketFnImport :: Maybe WebSocket -> Path Posix (Rel importLocation) (Dir C.ServerSrcDir) -> Aeson.Value
mkWebSocketFnImport maybeWebSocket relPathToServerSrcDir = extImportToImportJson relPathToServerSrcDir maybeWebSocketFn
  where
    maybeWebSocketFn = AS.App.WS.fn <$> maybeWebSocket
