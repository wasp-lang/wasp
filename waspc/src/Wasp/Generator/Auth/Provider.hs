{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Wasp.Generator.Auth.Provider
  ( OAuthProviderSpec (..),
    allOAuthProviders,
    enabledOAuthProviders,
    enabledAuthMethodsJson,
    isOAuthEnabled,
    isEmailEnabled,
    isUsernameAndPasswordEnabled,
    serverOAuthLoginUrl,
    serverOAuthCallbackUrl,
    clientOAuthCallbackPath,
    serverExchangeCodeForTokenUrl,
    oauthProviderScopeStr,
  )
where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import Data.Maybe (isJust, mapMaybe)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.Common (makeJsArrayFromHaskellList)

data OAuthProviderSpec = OAuthProviderSpec
  { slug :: String,
    displayName :: String,
    requiredScopes :: [String],
    extractConfig :: AS.Auth.AuthMethods -> Maybe AS.Auth.ExternalAuthConfig
  }

allOAuthProviders :: [OAuthProviderSpec]
allOAuthProviders =
  [ OAuthProviderSpec
      { slug = "google",
        displayName = "Google",
        requiredScopes = ["profile"],
        extractConfig = AS.Auth.google
      },
    OAuthProviderSpec
      { slug = "github",
        displayName = "GitHub",
        requiredScopes = [],
        extractConfig = AS.Auth.gitHub
      },
    OAuthProviderSpec
      { slug = "discord",
        displayName = "Discord",
        requiredScopes = ["identify"],
        extractConfig = AS.Auth.discord
      },
    OAuthProviderSpec
      { slug = "keycloak",
        displayName = "Keycloak",
        requiredScopes = ["profile"],
        extractConfig = AS.Auth.keycloak
      },
    OAuthProviderSpec
      { slug = "slack",
        displayName = "Slack",
        requiredScopes = ["openid"],
        extractConfig = AS.Auth.slack
      },
    OAuthProviderSpec
      { slug = "microsoft",
        displayName = "Microsoft",
        requiredScopes = ["openid", "profile", "email"],
        extractConfig = AS.Auth.microsoft
      }
  ]

enabledOAuthProviders :: AS.Auth.Auth -> [(OAuthProviderSpec, AS.Auth.ExternalAuthConfig)]
enabledOAuthProviders auth =
  mapMaybe tryEnable allOAuthProviders
  where
    methods = AS.Auth.methods auth
    tryEnable spec = case extractConfig spec methods of
      Just cfg -> Just (spec, cfg)
      Nothing -> Nothing

isOAuthEnabled :: AS.Auth.Auth -> Bool
isOAuthEnabled = not . null . enabledOAuthProviders

isEmailEnabled :: AS.Auth.Auth -> Bool
isEmailEnabled = isJust . AS.Auth.email . AS.Auth.methods

isUsernameAndPasswordEnabled :: AS.Auth.Auth -> Bool
isUsernameAndPasswordEnabled = isJust . AS.Auth.usernameAndPassword . AS.Auth.methods

enabledAuthMethodsJson :: AS.Auth.Auth -> Aeson.Value
enabledAuthMethodsJson auth =
  object $
    [ (providerJsonKey spec .= isProviderEnabled spec)
      | spec <- allOAuthProviders
    ]
      ++ [ "isUsernameAndPasswordAuthEnabled" .= isUsernameAndPasswordEnabled auth,
           "isEmailAuthEnabled" .= isEmailEnabled auth
         ]
  where
    methods = AS.Auth.methods auth
    isProviderEnabled spec = isJust $ extractConfig spec methods
    providerJsonKey spec = AesonKey.fromString $ "is" ++ displayName spec ++ "AuthEnabled"

serverOAuthLoginUrl :: OAuthProviderSpec -> String
serverOAuthLoginUrl spec = "/auth/" ++ slug spec ++ "/login"

serverOAuthCallbackUrl :: OAuthProviderSpec -> String
serverOAuthCallbackUrl spec = "/auth/" ++ slug spec ++ "/callback"

clientOAuthCallbackPath :: String
clientOAuthCallbackPath = "/oauth/callback"

serverExchangeCodeForTokenUrl :: String
serverExchangeCodeForTokenUrl = "/auth/exchange-code"

oauthProviderScopeStr :: OAuthProviderSpec -> String
oauthProviderScopeStr spec = makeJsArrayFromHaskellList (requiredScopes spec)
