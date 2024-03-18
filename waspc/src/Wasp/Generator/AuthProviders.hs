module Wasp.Generator.AuthProviders where

import Data.Maybe (fromJust)
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
