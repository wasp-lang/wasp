module Wasp.Generator.AuthProviders where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson
import Data.Maybe (fromJust)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders.Common (makeProviderId)
import qualified Wasp.Generator.AuthProviders.Email as E
import qualified Wasp.Generator.AuthProviders.Local as L
import qualified Wasp.Generator.AuthProviders.OAuth as OA

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

localAuthProvider :: L.LocalAuthProvider
localAuthProvider =
  L.LocalAuthProvider
    { L._providerId = fromJust $ makeProviderId "username",
      L._displayName = "Username and password"
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

microsoftEntraAuthProvider :: OA.OAuthAuthProvider
microsoftEntraAuthProvider =
  OA.OAuthAuthProvider
    { OA._providerId = fromJust $ makeProviderId "microsoftEntra",
      OA._displayName = "Microsoft Entra",
      OA._requiredScope = ["openid", "profile", "email"]
    }

getEnabledAuthProvidersJson :: AS.Auth.Auth -> Aeson.Value
getEnabledAuthProvidersJson auth =
  object
    [ "isSlackAuthEnabled" .= AS.Auth.isSlackAuthEnabled auth,
      "isDiscordAuthEnabled" .= AS.Auth.isDiscordAuthEnabled auth,
      "isGoogleAuthEnabled" .= AS.Auth.isGoogleAuthEnabled auth,
      "isKeycloakAuthEnabled" .= AS.Auth.isKeycloakAuthEnabled auth,
      "isGitHubAuthEnabled" .= AS.Auth.isGitHubAuthEnabled auth,
      "isMicrosoftEntraAuthEnabled" .= AS.Auth.isMicrosoftEntraAuthEnabled auth,
      "isUsernameAndPasswordAuthEnabled" .= AS.Auth.isUsernameAndPasswordAuthEnabled auth,
      "isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth
    ]
