module Wasp.Generator.WebAppGenerator.Auth.OAuthAuthG
  ( genOAuthAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath (File', Path', Rel', reldir, relfile)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders (gitHubAuthProvider, googleAuthProvider)
import Wasp.Generator.AuthProviders.OAuth (OAuthAuthProvider)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Util ((<++>))

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      genHelpers auth
        <++> sequence [genOAuthCodeExchange auth]
  | otherwise = return []

genHelpers :: AS.Auth.Auth -> Generator [FileDraft]
genHelpers auth =
  return $
    concat
      [ [gitHubHelpers | AS.Auth.isGitHubAuthEnabled auth],
        [googleHelpers | AS.Auth.isGoogleAuthEnabled auth]
      ]
  where
    gitHubHelpers = mkHelpersFd gitHubAuthProvider [relfile|GitHub.jsx|]
    googleHelpers = mkHelpersFd googleAuthProvider [relfile|Google.jsx|]

    mkHelpersFd :: OAuthAuthProvider -> Path' Rel' File' -> FileDraft
    mkHelpersFd provider helpersFp =
      mkTmplFdWithDstAndData
        [relfile|src/auth/helpers/Generic.jsx|]
        (SP.castRel $ [reldir|src/auth/helpers|] SP.</> helpersFp)
        (Just tmplData)
      where
        tmplData =
          object
            [ "signInPath" .= OAuth.serverLoginUrl provider,
              "iconName" .= (SP.fromRelFileP . fromJust . SP.relFileToPosix) (OAuth.logoFileName provider),
              "displayName" .= OAuth.displayName provider
            ]

genOAuthCodeExchange :: AS.Auth.Auth -> Generator FileDraft
genOAuthCodeExchange auth =
  return $
    C.mkTmplFdWithData
      [relfile|src/auth/pages/OAuthCodeExchange.jsx|]
      ( object
          [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
            "onAuthFailedRedirectTo" .= AS.Auth.onAuthFailedRedirectTo auth
          ]
      )
