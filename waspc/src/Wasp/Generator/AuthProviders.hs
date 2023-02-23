module Wasp.Generator.AuthProviders where

import StrongPath (relfile)
import Wasp.Generator.AuthProviders.Local (LocalAuthInfo, mkLocalAuthInfo)
import Wasp.Generator.AuthProviders.OAuth (ExternalAuthInfo, mkExternalAuthInfo)

googleAuthInfo :: ExternalAuthInfo
googleAuthInfo = mkExternalAuthInfo "google" "Google" [relfile|routes/auth/passport/google/config.js|] [relfile|google-logo-icon.png|]

gitHubAuthInfo :: ExternalAuthInfo
gitHubAuthInfo = mkExternalAuthInfo "github" "GitHub" [relfile|routes/auth/passport/github/config.js|] [relfile|github-logo-icon.png|]

localAuthInfo :: LocalAuthInfo
localAuthInfo = mkLocalAuthInfo "local" "Username and password"
