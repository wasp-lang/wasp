module Wasp.ServerRoutes.UserApi
  ( getUserApiRoutes,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import Wasp.ServerRoutes.ServerRoute
  ( ServerRoute (..),
    ServerRouteOwner (..),
    ServerRoutePath (..),
    getHttpMethodsRouteAnswersOn,
  )
import Wasp.Util.UrlPath (getStaticPathPrefix, stripTrailingSlashes)

-- | The user declares these, we only describe them here. An api path can be an Express
-- pattern (e.g. "/files/:id"). We do not interpret those: such an api claims everything
-- under the static beginning of its path.
getUserApiRoutes :: AppSpec -> [ServerRoute]
getUserApiRoutes = map makeUserApiRoute . AS.getApis
  where
    makeUserApiRoute (apiName, api) =
      ServerRoute
        { owner = UserApiRoute apiName,
          httpMethods = getHttpMethodsRouteAnswersOn (AS.Api.method api),
          path = getUserApiRoutePath (AS.Api.path api)
        }

    getUserApiRoutePath apiPath
      | staticPathPrefix == stripTrailingSlashes apiPath = ExactPath apiPath
      | otherwise = SubtreePath staticPathPrefix
      where
        staticPathPrefix = getStaticPathPrefix apiPath
