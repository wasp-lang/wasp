-- | Every route the generated server answers on, described in one place.
--
-- Generators render route paths from here, and the validators (and anyone who needs to know
-- which paths belong to the server) read 'getWaspServerRoutes' and 'getServerRoutes'.
--
-- Each section below holds one domain's route names and ends with the list of its routes.
-- A route that is not in its domain's list does not exist for the readers of this module,
-- so when you add a route, add it to the list too.
--
-- This module sits below "Wasp.AppSpec.Valid", so it must not import it.
module Wasp.ServerRoutes
  ( -- * Server route
    ServerRoute (..),
    ServerRouteOwner (..),
    ServerRoutePath (..),
    ServerRouteHttpMethods (..),
    getRoutePath,
    getRoutePathWithoutLeadingSlash,
    doRoutesOverlap,

    -- * All routes
    getServerRoutes,
    getWaspServerRoutes,
    getUserApiRoutes,
    mayServerHaveRoutesUnknownToWasp,

    -- * WebSocket
    webSocketRouteInHttpServer,
    webSocketRoute,

    -- * Auth
    authRouteInRootRouter,
    meRouteInAuthRouter,
    logoutRouteInAuthRouter,
    exchangeCodeRouteInAuthRouter,
    loginRouteInAuthProviderRouter,
    signupRouteInAuthProviderRouter,
    callbackRouteInAuthProviderRouter,
    requestPasswordResetRouteInAuthProviderRouter,
    resetPasswordRouteInAuthProviderRouter,
    verifyEmailRouteInAuthProviderRouter,
    meRoute,
    logoutRoute,
    exchangeCodeRoute,
    oAuthLoginRoute,
    oAuthCallbackRoute,
    usernameLoginRoute,
    usernameSignupRoute,
    emailLoginRoute,
    emailSignupRoute,
    emailRequestPasswordResetRoute,
    emailResetPasswordRoute,
    emailVerifyEmailRoute,
    getAuthRoutes,

    -- * Operations
    operationsRouteInRootRouter,
    operationRouteInOperationsRouter,
    operationRoute,

    -- * Crud
    crudRouteInRootRouter,
    getCrudOperationRouterRoute,
    crudOperationRouteInCrudRouter,
    crudOperationRoute,

    -- * Liveness
    upRouteInRootRouter,
    upRoute,
  )
where

import Data.Char (toLower)
import Data.List (intercalate, intersect)
import Data.Maybe (isJust)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Server as AS.App.Server
import qualified Wasp.AppSpec.Crud as AS.Crud
import qualified Wasp.AppSpec.Operation as AS.Operation
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.AuthProviders.Email as Email
import qualified Wasp.Generator.AuthProviders.Local as Local
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Util (camelToKebabCase)
import Wasp.Util.UrlPath (getStaticPathPrefix, isPathSegmentPrefixOf, stripTrailingSlashes)

-- * Server route

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

-- | E.g. "operations/get-tasks". The SDK names routes relative to the server's url.
getRoutePathWithoutLeadingSlash :: ServerRoute -> String
getRoutePathWithoutLeadingSlash = dropWhile (== '/') . getRoutePath

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

makeWaspRoute :: ServerRouteOwner -> AS.Api.HttpMethod -> [String] -> ServerRoute
makeWaspRoute routeOwner httpMethod routeSegments =
  ServerRoute
    { owner = routeOwner,
      httpMethods = getHttpMethodsRouteAnswersOn httpMethod,
      path = ExactPath $ makePathFromSegments routeSegments
    }

-- | Express answers a `HEAD` request with the path's `GET` route.
getHttpMethodsRouteAnswersOn :: AS.Api.HttpMethod -> ServerRouteHttpMethods
getHttpMethodsRouteAnswersOn AS.Api.ALL = AnyHttpMethod
getHttpMethodsRouteAnswersOn AS.Api.GET = OnlyHttpMethods [AS.Api.GET, AS.Api.HEAD]
getHttpMethodsRouteAnswersOn httpMethod = OnlyHttpMethods [httpMethod]

makePathFromSegments :: [String] -> String
makePathFromSegments segments = "/" ++ intercalate "/" segments

-- * All routes

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
mayServerHaveRoutesUnknownToWasp spec = isJust $ getApp spec >>= AS.App.server >>= AS.App.Server.setupFn

-- | We cannot use @getApp@ from "Wasp.AppSpec.Valid", because that module imports this one.
getApp :: AppSpec -> Maybe AS.App.App
getApp spec = snd <$> AS.getApp (AS.decls spec)

-- * WebSocket

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
    areWebSocketsUsed = isJust $ getApp spec >>= AS.App.webSocket

-- * Auth

authRouteInRootRouter :: String
authRouteInRootRouter = "auth"

meRouteInAuthRouter :: String
meRouteInAuthRouter = "me"

logoutRouteInAuthRouter :: String
logoutRouteInAuthRouter = "logout"

exchangeCodeRouteInAuthRouter :: String
exchangeCodeRouteInAuthRouter = "exchange-code"

loginRouteInAuthProviderRouter :: String
loginRouteInAuthProviderRouter = "login"

signupRouteInAuthProviderRouter :: String
signupRouteInAuthProviderRouter = "signup"

callbackRouteInAuthProviderRouter :: String
callbackRouteInAuthProviderRouter = "callback"

requestPasswordResetRouteInAuthProviderRouter :: String
requestPasswordResetRouteInAuthProviderRouter = "request-password-reset"

resetPasswordRouteInAuthProviderRouter :: String
resetPasswordRouteInAuthProviderRouter = "reset-password"

verifyEmailRouteInAuthProviderRouter :: String
verifyEmailRouteInAuthProviderRouter = "verify-email"

meRoute :: ServerRoute
meRoute = makeWaspRoute AuthRoute AS.Api.GET [authRouteInRootRouter, meRouteInAuthRouter]

logoutRoute :: ServerRoute
logoutRoute = makeWaspRoute AuthRoute AS.Api.POST [authRouteInRootRouter, logoutRouteInAuthRouter]

-- | Where the client trades the one-time code from an OAuth login for a session.
exchangeCodeRoute :: ServerRoute
exchangeCodeRoute = makeWaspRoute AuthRoute AS.Api.POST [authRouteInRootRouter, exchangeCodeRouteInAuthRouter]

oAuthLoginRoute :: OAuth.OAuthAuthProvider -> ServerRoute
oAuthLoginRoute provider = makeAuthProviderRoute (OAuth.providerId provider) AS.Api.GET loginRouteInAuthProviderRouter

oAuthCallbackRoute :: OAuth.OAuthAuthProvider -> ServerRoute
oAuthCallbackRoute provider = makeAuthProviderRoute (OAuth.providerId provider) AS.Api.GET callbackRouteInAuthProviderRouter

usernameLoginRoute :: ServerRoute
usernameLoginRoute = makeUsernameRoute loginRouteInAuthProviderRouter

usernameSignupRoute :: ServerRoute
usernameSignupRoute = makeUsernameRoute signupRouteInAuthProviderRouter

emailLoginRoute :: ServerRoute
emailLoginRoute = makeEmailRoute loginRouteInAuthProviderRouter

emailSignupRoute :: ServerRoute
emailSignupRoute = makeEmailRoute signupRouteInAuthProviderRouter

emailRequestPasswordResetRoute :: ServerRoute
emailRequestPasswordResetRoute = makeEmailRoute requestPasswordResetRouteInAuthProviderRouter

emailResetPasswordRoute :: ServerRoute
emailResetPasswordRoute = makeEmailRoute resetPasswordRouteInAuthProviderRouter

emailVerifyEmailRoute :: ServerRoute
emailVerifyEmailRoute = makeEmailRoute verifyEmailRouteInAuthProviderRouter

makeUsernameRoute :: String -> ServerRoute
makeUsernameRoute = makeAuthProviderRoute (Local.providerId AuthProviders.localAuthProvider) AS.Api.POST

makeEmailRoute :: String -> ServerRoute
makeEmailRoute = makeAuthProviderRoute (Email.providerId AuthProviders.emailAuthProvider) AS.Api.POST

-- | The auth router mounts one router per provider, under the provider's id.
makeAuthProviderRoute :: String -> AS.Api.HttpMethod -> String -> ServerRoute
makeAuthProviderRoute providerId httpMethod routeInAuthProviderRouter =
  makeWaspRoute (AuthProviderRoute providerId) httpMethod [authRouteInRootRouter, providerId, routeInAuthProviderRouter]

getAuthRoutes :: AppSpec -> [ServerRoute]
getAuthRoutes spec = maybe [] getRoutesOfAuth (getApp spec >>= AS.App.auth)
  where
    getRoutesOfAuth auth =
      concat
        [ [meRoute, logoutRoute],
          [exchangeCodeRoute | AS.Auth.isExternalAuthEnabled auth],
          concat
            [ [oAuthLoginRoute provider, oAuthCallbackRoute provider]
            | provider <- AuthProviders.getEnabledOAuthProviders auth
            ],
          concat [[usernameLoginRoute, usernameSignupRoute] | AS.Auth.isUsernameAndPasswordAuthEnabled auth],
          concat [emailRoutes | AS.Auth.isEmailAuthEnabled auth]
        ]

    emailRoutes =
      [ emailLoginRoute,
        emailSignupRoute,
        emailRequestPasswordResetRoute,
        emailResetPasswordRoute,
        emailVerifyEmailRoute
      ]

-- * Operations

operationsRouteInRootRouter :: String
operationsRouteInRootRouter = "operations"

operationRouteInOperationsRouter :: AS.Operation.Operation -> String
operationRouteInOperationsRouter = camelToKebabCase . AS.Operation.getName

operationRoute :: AS.Operation.Operation -> ServerRoute
operationRoute operation =
  makeWaspRoute
    (OperationRoute $ AS.Operation.getName operation)
    AS.Api.POST
    [operationsRouteInRootRouter, operationRouteInOperationsRouter operation]

getOperationRoutes :: AppSpec -> [ServerRoute]
getOperationRoutes = map operationRoute . AS.getOperations

-- * Crud

crudRouteInRootRouter :: String
crudRouteInRootRouter = "crud"

getCrudOperationRouterRoute :: String -> String
getCrudOperationRouterRoute crudName = crudName

crudOperationRouteInCrudRouter :: AS.Crud.CrudOperation -> String
crudOperationRouteInCrudRouter operation = case operation of
  AS.Crud.Get -> "get"
  AS.Crud.GetAll -> "get-all"
  AS.Crud.Create -> "create"
  AS.Crud.Update -> "update"
  AS.Crud.Delete -> "delete"

crudOperationRoute :: String -> AS.Crud.CrudOperation -> ServerRoute
crudOperationRoute crudName crudOperation =
  makeWaspRoute
    (CrudRoute crudName)
    AS.Api.POST
    [crudRouteInRootRouter, getCrudOperationRouterRoute crudName, crudOperationRouteInCrudRouter crudOperation]

getCrudRoutes :: AppSpec -> [ServerRoute]
getCrudRoutes spec =
  [ crudOperationRoute crudName crudOperation
  | (crudName, crud) <- AS.getCruds spec,
    (crudOperation, _) <- AS.Crud.toOperationList (AS.Crud.operations crud)
  ]

-- * Liveness

-- | The liveness check, the same route Rails and Laravel use.
upRouteInRootRouter :: String
upRouteInRootRouter = "up"

upRoute :: ServerRoute
upRoute = makeWaspRoute LivenessRoute AS.Api.GET [upRouteInRootRouter]

getLivenessRoutes :: [ServerRoute]
getLivenessRoutes = [upRoute]

-- * User apis

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
