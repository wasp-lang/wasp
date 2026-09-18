module Wasp.ServerRoutes.WebSocket
  ( webSocketRouteInHttpServer,
    webSocketRoute,
    getWebSocketRoutes,
  )
where

import Data.Maybe (isJust)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import Wasp.ServerRoutes.ServerRoute
  ( ServerRoute (..),
    ServerRouteHttpMethods (..),
    ServerRouteOwner (..),
    ServerRoutePath (..),
    makePathFromSegments,
  )

-- | Socket.IO attaches to the http server, ahead of Express, and answers everything under
-- this path. This is also Socket.IO's default, but we set it on both ends to be explicit.
webSocketRouteInHttpServer :: String
webSocketRouteInHttpServer = "socket.io"

webSocketRoute :: ServerRoute
webSocketRoute =
  ServerRoute
    { owner = WebSocketRoute,
      httpMethods = AnyHttpMethod,
      path = SubtreePath $ makePathFromSegments [webSocketRouteInHttpServer]
    }

getWebSocketRoutes :: AppSpec -> [ServerRoute]
getWebSocketRoutes spec = [webSocketRoute | areWebSocketsUsed]
  where
    areWebSocketsUsed = isJust $ AS.getApp (AS.decls spec) >>= AS.App.webSocket . snd
