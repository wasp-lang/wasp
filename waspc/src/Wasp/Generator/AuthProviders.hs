module Wasp.Generator.AuthProviders where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders.Common (makeProviderId)
import qualified Wasp.Generator.AuthProviders.Email as E
import qualified Wasp.Generator.AuthProviders.OAuth as OA
import qualified Wasp.Generator.AuthProviders.UsernameAndPassword as UP

googleAuthProvider :: OA.OAuthAuthProvider
googleAuthProvider =
  OA.OAuthAuthProvider
    { OA._providerId = fromJust $ makeProviderId "google",
      OA._displayName = "Google",
      OA._requiredScope = ["profile"]
    }

keycloakAuthProvider :: OA.OAuthAuthProvider
keycloakAuthProvider =
  OA.OAuthAuthProvider
    { OA._providerId = fromJust $ makeProviderId "keycloak",
      OA._displayName = "Keycloak",
      OA._requiredScope = ["profile"]
    }

gitHubAuthProvider :: OA.OAuthAuthProvider
gitHubAuthProvider =
  OA.OAuthAuthProvider
    { OA._providerId = fromJust $ makeProviderId "github",
      OA._displayName = "GitHub",
      OA._requiredScope = []
    }

usernameAndPasswordAuthProvider :: UP.UsernameAndPasswordAuthProvider
usernameAndPasswordAuthProvider =
  UP.UsernameAndPasswordAuthProvider
    { UP._providerId = fromJust $ makeProviderId "username",
      UP._displayName = "Username and password"
    }

emailAuthProvider :: E.EmailAuthProvider
emailAuthProvider =
  E.EmailAuthProvider
    { E._providerId = fromJust $ makeProviderId "email",
      E._displayName = "Email and password"
    }

slackAuthProvider :: OA.OAuthAuthProvider
slackAuthProvider =
  OA.OAuthAuthProvider
    { OA._providerId = fromJust $ makeProviderId "slack",
      OA._displayName = "Slack",
      OA._requiredScope = ["openid"]
    }

discordAuthProvider :: OA.OAuthAuthProvider
discordAuthProvider =
  OA.OAuthAuthProvider
    { OA._providerId = fromJust $ makeProviderId "discord",
      OA._displayName = "Discord",
      OA._requiredScope = ["identify"]
    }

microsoftAuthProvider :: OA.OAuthAuthProvider
microsoftAuthProvider =
  OA.OAuthAuthProvider
    { OA._providerId = fromJust $ makeProviderId "microsoft",
      OA._displayName = "Microsoft",
      OA._requiredScope = ["openid", "profile", "email"]
    }

-- | The enabled providers in the order the auth libs list a user's identities
-- (e.g. `AuthUser.identities`, `getFirstProviderUserId`), as a list of
-- objects with a `providerName` field.
getEnabledAuthProvidersListJson :: AS.Auth.Auth -> Aeson.Value
getEnabledAuthProvidersListJson auth = makeProviderNamesJson $ getEnabledAuthProviderIds auth

getEnabledOAuthProvidersListJson :: AS.Auth.Auth -> Aeson.Value
getEnabledOAuthProvidersListJson auth = makeProviderNamesJson $ getEnabledOAuthProviderIds auth

makeProviderNamesJson :: [String] -> Aeson.Value
makeProviderNamesJson providerIds = Aeson.toJSON $ map (\pid -> object ["providerName" .= pid]) providerIds

getEnabledAuthProviderIds :: AS.Auth.Auth -> [String]
getEnabledAuthProviderIds auth =
  concat
    [ [E.providerId emailAuthProvider | AS.Auth.isEmailAuthEnabled auth],
      [UP.providerId usernameAndPasswordAuthProvider | AS.Auth.isUsernameAndPasswordAuthEnabled auth]
    ]
    ++ getEnabledOAuthProviderIds auth

getEnabledOAuthProviderIds :: AS.Auth.Auth -> [String]
getEnabledOAuthProviderIds auth =
  concat
    [ [OA.providerId slackAuthProvider | AS.Auth.isSlackAuthEnabled auth],
      [OA.providerId discordAuthProvider | AS.Auth.isDiscordAuthEnabled auth],
      [OA.providerId googleAuthProvider | AS.Auth.isGoogleAuthEnabled auth],
      [OA.providerId keycloakAuthProvider | AS.Auth.isKeycloakAuthEnabled auth],
      [OA.providerId gitHubAuthProvider | AS.Auth.isGitHubAuthEnabled auth],
      [OA.providerId microsoftAuthProvider | AS.Auth.isMicrosoftAuthEnabled auth]
    ]

getEnabledAuthProvidersJson :: AS.Auth.Auth -> Aeson.Value
getEnabledAuthProvidersJson auth =
  object
    [ "isSlackAuthEnabled" .= AS.Auth.isSlackAuthEnabled auth,
      "isDiscordAuthEnabled" .= AS.Auth.isDiscordAuthEnabled auth,
      "isGoogleAuthEnabled" .= AS.Auth.isGoogleAuthEnabled auth,
      "isKeycloakAuthEnabled" .= AS.Auth.isKeycloakAuthEnabled auth,
      "isGitHubAuthEnabled" .= AS.Auth.isGitHubAuthEnabled auth,
      "isMicrosoftAuthEnabled" .= AS.Auth.isMicrosoftAuthEnabled auth,
      "isUsernameAndPasswordAuthEnabled" .= AS.Auth.isUsernameAndPasswordAuthEnabled auth,
      "isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth
    ]
