module Wasp.Generator.AuthProviders where

import Data.Maybe (fromJust)
import StrongPath (relfile)
import qualified Wasp.AppSpec.App.Dependency as App.Dependency
import Wasp.Generator.AuthProviders.Common (makeProviderId)
import qualified Wasp.Generator.AuthProviders.Email as E
import qualified Wasp.Generator.AuthProviders.Local as L
import qualified Wasp.Generator.AuthProviders.OAuth as OA

googleAuthProvider :: OA.OAuthAuthProvider
googleAuthProvider =
  OA.OAuthAuthProvider
    { OA._providerId = fromJust $ makeProviderId "google",
      OA._displayName = "Google",
      OA._requiredScope = ["profile"],
      OA._logoFileName = [relfile|google-logo-icon.png|],
      OA._passportDependency = App.Dependency.make ("passport-google-oauth20", "2.0.0")
    }

gitHubAuthProvider :: OA.OAuthAuthProvider
gitHubAuthProvider =
  OA.OAuthAuthProvider
    { OA._providerId = fromJust $ makeProviderId "github",
      OA._displayName = "GitHub",
      OA._requiredScope = [],
      OA._logoFileName = [relfile|github-logo-icon.png|],
      OA._passportDependency = App.Dependency.make ("passport-github2", "0.1.12")
    }

localAuthProvider :: L.LocalAuthProvider
localAuthProvider =
  L.LocalAuthProvider
    { L._providerId = fromJust $ makeProviderId "local",
      L._displayName = "Username and password"
    }

emailAuthProvider :: E.EmailAuthProvider
emailAuthProvider =
  E.EmailAuthProvider
    { E._providerId = fromJust $ makeProviderId "email",
      E._displayName = "Email and password"
    }