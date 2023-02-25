module Wasp.Generator.AuthProviders where

import StrongPath (relfile)
import qualified Wasp.AppSpec.App.Dependency as App.Dependency
import Wasp.Generator.AuthProviders.Local (LocalAuthInfo, mkLocalAuthInfo)
import Wasp.Generator.AuthProviders.OAuth (ExternalAuthInfo, mkExternalAuthInfo)

googleAuthInfo :: ExternalAuthInfo
googleAuthInfo =
  mkExternalAuthInfo
    "google"
    "Google"
    [relfile|auth/passport/google/config.js|]
    [relfile|google-logo-icon.png|]
    $ App.Dependency.make ("passport-google-oauth20", "2.0.0")

gitHubAuthInfo :: ExternalAuthInfo
gitHubAuthInfo =
  mkExternalAuthInfo
    "github"
    "GitHub"
    [relfile|auth/passport/github/config.js|]
    [relfile|github-logo-icon.png|]
    $ App.Dependency.make ("passport-github2", "0.1.12")

localAuthInfo :: LocalAuthInfo
localAuthInfo =
  mkLocalAuthInfo
    "local"
    "Username and password"
