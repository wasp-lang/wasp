module Wasp.ServerRoutes.Liveness
  ( upRouteInRootRouter,
    upRoute,
    getLivenessRoutes,
  )
where

import qualified Wasp.AppSpec.Api as AS.Api
import Wasp.ServerRoutes.ServerRoute (ServerRoute, ServerRouteOwner (..), makeWaspRouteInRootRouter)

-- | Since our health check is just a simple liveness check,
-- we use the same @/up@ route that Rails and Laravel use.
--
-- Health checks (@/health@ route) are much more complex,
-- so we let users handle it themselves.
upRouteInRootRouter :: String
upRouteInRootRouter = "up"

upRoute :: ServerRoute
upRoute = makeWaspRouteInRootRouter LivenessRoute AS.Api.GET [upRouteInRootRouter]

getLivenessRoutes :: [ServerRoute]
getLivenessRoutes = [upRoute]
