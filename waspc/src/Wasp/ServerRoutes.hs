-- | Every route the generated server answers on.
--
-- Each domain describes its routes in its own module under @Wasp.ServerRoutes@, and ends
-- with the list of them (@get<Domain>Routes@). Here we only put those lists together.
-- Generators render route paths from the domain modules, and the validators (and anyone who
-- needs to know which paths belong to the server) read the lists from here.
--
-- A route that is not in its domain's list does not exist for the readers of this module,
-- so when you add a route, add it to the list too. When you add a domain, add it here.
--
-- These modules sit below "Wasp.AppSpec.Valid", so they must not import it.
module Wasp.ServerRoutes
  ( getServerRoutes,
    getWaspServerRoutes,
    mayServerHaveRoutesUnknownToWasp,
  )
where

import Data.Maybe (isJust)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import Wasp.ServerRoutes.Auth (getAuthRoutes)
import Wasp.ServerRoutes.Crud (getCrudRoutes)
import Wasp.ServerRoutes.Liveness (getLivenessRoutes)
import Wasp.ServerRoutes.Operations (getOperationRoutes)
import Wasp.ServerRoutes.ServerRoute (ServerRoute)
import Wasp.ServerRoutes.UserApi (getUserApiRoutes)
import Wasp.ServerRoutes.WebSocket (getWebSocketRoutes)

-- | Every route the compiler can see: Wasp's own routes, then the user's apis.
-- The server registers them in this order, so an earlier route shadows a later one.
getServerRoutes :: AppSpec -> [ServerRoute]
getServerRoutes spec = getWaspServerRoutes spec ++ getUserApiRoutes spec

-- | Every route Wasp registers itself.
--
-- The wrong-port page at `GET /` is left out on purpose: it exists only in development, is
-- registered after the user's apis, and nobody should treat `/` as the server's.
getWaspServerRoutes :: AppSpec -> [ServerRoute]
getWaspServerRoutes spec =
  concat
    [ getWebSocketRoutes spec,
      getAuthRoutes spec,
      getOperationRoutes spec,
      getCrudRoutes spec,
      getLivenessRoutes
    ]

-- | The server's `setupFn` receives the Express app, so it can register routes that the
-- compiler cannot see.
mayServerHaveRoutesUnknownToWasp :: AppSpec -> Bool
mayServerHaveRoutesUnknownToWasp spec = isJust $ AS.getApp (AS.decls spec) >>= AS.App.server . snd >>= AS.App.Server.setupFn
