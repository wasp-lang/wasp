module Wasp.Generator.SdkGenerator.Auth.OAuthAuthG
  ( genOAuthAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (File', Path', Rel', reldir, relfile)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders
  ( discordAuthProvider,
    gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
    microsoftAuthProvider,
    slackAuthProvider,
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
      [ [slackHelpers | AS.Auth.isSlackAuthEnabled auth],
        [discordHelpers | AS.Auth.isDiscordAuthEnabled auth],
        [gitHubHelpers | AS.Auth.isGitHubAuthEnabled auth],
        [googleHelpers | AS.Auth.isGoogleAuthEnabled auth],
        [keycloakHelpers | AS.Auth.isKeycloakAuthEnabled auth],
        [microsoftHelpers | AS.Auth.isMicrosoftAuthEnabled auth]
      ]
  where
    slackHelpers = mkHelpersFd slackAuthProvider [relfile|Slack.tsx|]
    discordHelpers = mkHelpersFd discordAuthProvider [relfile|Discord.tsx|]
    gitHubHelpers = mkHelpersFd gitHubAuthProvider [relfile|GitHub.tsx|]
    googleHelpers = mkHelpersFd googleAuthProvider [relfile|Google.tsx|]
    keycloakHelpers = mkHelpersFd keycloakAuthProvider [relfile|Keycloak.tsx|]
    microsoftHelpers = mkHelpersFd microsoftAuthProvider [relfile|Microsoft.tsx|]

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
