module Wasp.ServerRoutes.Auth
  ( authRouteInRootRouter,
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
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.Generator.AuthProviders as AuthProviders
import qualified Wasp.Generator.AuthProviders.Email as Email
import qualified Wasp.Generator.AuthProviders.Local as Local
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.ServerRoutes.ServerRoute (ServerRoute, ServerRouteOwner (..), makeWaspRouteInNestedRouter)

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
meRoute = makeWaspRouteInNestedRouter AuthRoute AS.Api.GET [authRouteInRootRouter, meRouteInAuthRouter]

logoutRoute :: ServerRoute
logoutRoute = makeWaspRouteInNestedRouter AuthRoute AS.Api.POST [authRouteInRootRouter, logoutRouteInAuthRouter]

-- | Where the client trades the one-time code from an OAuth login for a session.
exchangeCodeRoute :: ServerRoute
exchangeCodeRoute = makeWaspRouteInNestedRouter AuthRoute AS.Api.POST [authRouteInRootRouter, exchangeCodeRouteInAuthRouter]

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
  makeWaspRouteInNestedRouter (AuthProviderRoute providerId) httpMethod [authRouteInRootRouter, providerId, routeInAuthProviderRouter]

getAuthRoutes :: AppSpec -> [ServerRoute]
getAuthRoutes spec = maybe [] getRoutesOfAuth (AS.getApp (AS.decls spec) >>= AS.App.auth . snd)
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
