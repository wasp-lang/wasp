module Wasp.Generator.AuthProviders where

import StrongPath (relfile)
import qualified Wasp.AppSpec.App.Dependency as App.Dependency
import qualified Wasp.Generator.AuthProviders.Local as L
import qualified Wasp.Generator.AuthProviders.OAuth as OA

googleAuthInfo :: OA.OAuthAuthInfo
googleAuthInfo =
  OA.OAuthAuthInfo
    { OA._providerId = "google",
      OA._displayName = "Google",
      OA._requiredScope = ["profile"],
      OA._logoFileName = [relfile|google-logo-icon.png|],
      OA._passportDependency = App.Dependency.make ("passport-google-oauth20", "2.0.0")
    }

gitHubAuthInfo :: OA.OAuthAuthInfo
gitHubAuthInfo =
  OA.OAuthAuthInfo
    { OA._providerId = "github",
      OA._displayName = "GitHub",
      OA._requiredScope = [],
      OA._logoFileName = [relfile|github-logo-icon.png|],
      OA._passportDependency = App.Dependency.make ("passport-github2", "0.1.12")
    }

localAuthInfo :: L.LocalAuthInfo
localAuthInfo =
  L.LocalAuthInfo
    { L._providerId = "local",
      L._displayName = "Username and password"
    }
