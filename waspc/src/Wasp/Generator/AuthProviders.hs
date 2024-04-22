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

getEnabledAuthProvidersJson :: AS.Auth.Auth -> Aeson.Value
getEnabledAuthProvidersJson auth =
  object
    [ "isGoogleAuthEnabled" .= AS.Auth.isGoogleAuthEnabled auth,
      "isKeycloakAuthEnabled" .= AS.Auth.isKeycloakAuthEnabled auth,
      "isGitHubAuthEnabled" .= AS.Auth.isGitHubAuthEnabled auth,
      "isUsernameAndPasswordAuthEnabled" .= AS.Auth.isUsernameAndPasswordAuthEnabled auth,
      "isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth
    ]
