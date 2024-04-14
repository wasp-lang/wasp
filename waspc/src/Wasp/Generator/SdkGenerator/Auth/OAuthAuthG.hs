module Wasp.Generator.SdkGenerator.Auth.OAuthAuthG
  ( genOAuthAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (File', Path', Rel', reldir, relfile)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders
  ( gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
  )
import Wasp.Generator.AuthProviders.OAuth (OAuthAuthProvider)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common as C

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      genHelpers auth
  | otherwise = return []

genHelpers :: AS.Auth.Auth -> Generator [FileDraft]
genHelpers auth =
  return $
    concat
      [ [gitHubHelpers | AS.Auth.isGitHubAuthEnabled auth],
        [googleHelpers | AS.Auth.isGoogleAuthEnabled auth],
        [keycloakHelpers | AS.Auth.isKeycloakAuthEnabled auth]
      ]
  where
    gitHubHelpers = mkHelpersFd gitHubAuthProvider [relfile|GitHub.tsx|]
    googleHelpers = mkHelpersFd googleAuthProvider [relfile|Google.tsx|]
    keycloakHelpers = mkHelpersFd keycloakAuthProvider [relfile|Keycloak.tsx|]

    mkHelpersFd :: OAuthAuthProvider -> Path' Rel' File' -> FileDraft
    mkHelpersFd provider helpersFp =
      mkTmplFdWithDstAndData
        [relfile|auth/helpers/_Provider.tsx|]
        (SP.castRel $ [reldir|auth/helpers|] SP.</> helpersFp)
        (Just tmplData)
      where
        tmplData =
          object
            [ "signInPath" .= OAuth.serverLoginUrl provider,
              "displayName" .= OAuth.displayName provider
            ]
