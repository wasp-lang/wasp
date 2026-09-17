-- | How we describe one route of the generated server.
module Wasp.ServerRoutes.ServerRoute
  ( ServerRoute (..),
    ServerRouteOwner (..),
    ServerRoutePath (..),
    ServerRouteHttpMethods (..),
    makeWaspRouteInRootRouter,
    makeWaspRouteInNestedRouter,
    getHttpMethodsRouteAnswersOn,
    makePathFromSegments,
    getRoutePath,
    doRoutesOverlap,
  )
where

import Data.Char (toLower)
import Data.List (intercalate, intersect)
import qualified Wasp.AppSpec.Api as AS.Api
import Wasp.Util.UrlPath (isPathSegmentPrefixOf, stripTrailingSlashes)

data ServerRoute = ServerRoute
  { owner :: ServerRouteOwner,
    httpMethods :: ServerRouteHttpMethods,
    path :: ServerRoutePath
  }
  deriving (Show, Eq)

-- | Who registers the route. Holds the name of the declaration or the provider it comes from.
data ServerRouteOwner
  = WebSocketRoute
  | AuthRoute
  | AuthProviderRoute String
  | OperationRoute String
  | CrudRoute String
  | LivenessRoute
  | UserApiRoute String
  deriving (Show, Eq)

data ServerRoutePath
  = -- | A path we can spell out in full, e.g. "/auth/me".
    ExactPath String
  | -- | A path under which we cannot name the routes, so the route claims everything below
    -- it, e.g. "/socket.io", or "/files" for a user's api at "/files/:id".
    SubtreePath String
  deriving (Show, Eq)

data ServerRouteHttpMethods
  = -- | An `ALL` api, or a subtree whose routes (and so their methods) we cannot name.
    AnyHttpMethod
  | OnlyHttpMethods [AS.Api.HttpMethod]
  deriving (Show, Eq)

-- | E.g. "/operations/get-tasks".
getRoutePath :: ServerRoute -> String
getRoutePath route = case path route of
  ExactPath exactPath -> exactPath
  SubtreePath subtreePath -> subtreePath

-- | Whether some request could match both routes, so that the one registered first shadows
-- the other. Express matches paths case-insensitively and ignores trailing slashes.
doRoutesOverlap :: ServerRoute -> ServerRoute -> Bool
doRoutesOverlap routeA routeB =
  doHttpMethodsOverlap (httpMethods routeA) (httpMethods routeB)
    && doPathsOverlap (path routeA) (path routeB)
  where
    doHttpMethodsOverlap AnyHttpMethod _ = True
    doHttpMethodsOverlap _ AnyHttpMethod = True
    doHttpMethodsOverlap (OnlyHttpMethods methodsA) (OnlyHttpMethods methodsB) = not . null $ methodsA `intersect` methodsB

    doPathsOverlap (ExactPath pathA) (ExactPath pathB) = normalizePath pathA == normalizePath pathB
    doPathsOverlap (SubtreePath subtreePath) (ExactPath exactPath) = normalizePath subtreePath `isPathSegmentPrefixOf` normalizePath exactPath
    doPathsOverlap exactPath@(ExactPath _) subtreePath@(SubtreePath _) = doPathsOverlap subtreePath exactPath
    doPathsOverlap (SubtreePath pathA) (SubtreePath pathB) =
      normalizePath pathA `isPathSegmentPrefixOf` normalizePath pathB
        || normalizePath pathB `isPathSegmentPrefixOf` normalizePath pathA

    normalizePath = map toLower . stripTrailingSlashes

-- | A route that Wasp registers directly in the root router, next to the user's apis.
makeWaspRouteInRootRouter :: ServerRouteOwner -> AS.Api.HttpMethod -> [String] -> ServerRoute
makeWaspRouteInRootRouter routeOwner httpMethod routeSegments =
  ServerRoute
    { owner = routeOwner,
      httpMethods = getHttpMethodsRouteAnswersOn httpMethod,
      path = ExactPath $ makePathFromSegments routeSegments
    }

-- | A route that Wasp registers in a router of its own, mounted in the root router.
-- Such a router answers an `OPTIONS` request for each of its routes itself,
-- so the request never reaches the user's apis.
makeWaspRouteInNestedRouter :: ServerRouteOwner -> AS.Api.HttpMethod -> [String] -> ServerRoute
makeWaspRouteInNestedRouter routeOwner httpMethod routeSegments =
  route {httpMethods = httpMethods route `addHttpMethod` AS.Api.OPTIONS}
  where
    route = makeWaspRouteInRootRouter routeOwner httpMethod routeSegments

    addHttpMethod AnyHttpMethod _ = AnyHttpMethod
    addHttpMethod (OnlyHttpMethods methods) method = OnlyHttpMethods $ methods ++ [method]

-- | Express answers a `HEAD` request with the path's `GET` route.
getHttpMethodsRouteAnswersOn :: AS.Api.HttpMethod -> ServerRouteHttpMethods
getHttpMethodsRouteAnswersOn AS.Api.ALL = AnyHttpMethod
getHttpMethodsRouteAnswersOn AS.Api.GET = OnlyHttpMethods [AS.Api.GET, AS.Api.HEAD]
getHttpMethodsRouteAnswersOn httpMethod = OnlyHttpMethods [httpMethod]

makePathFromSegments :: [String] -> String
makePathFromSegments segments = "/" ++ intercalate "/" segments
