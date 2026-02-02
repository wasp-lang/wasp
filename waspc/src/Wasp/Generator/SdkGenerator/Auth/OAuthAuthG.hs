module Wasp.Generator.SdkGenerator.Auth.OAuthAuthG
  ( genOAuthAuth,
  )
where

import Data.Aeson (object, (.=))
import Data.List (filter)
import StrongPath (File', Path', Rel', reldir, relfile)
import qualified StrongPath as SP
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders
  ( discordAuthProvider,
    gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
    microsoftEntraAuthProvider,
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
        [microsoftEntraHelpers | AS.Auth.isMicrosoftEntraAuthEnabled auth]
      ]
  where
    slackHelpers = mkHelpersFd slackAuthProvider [relfile|Slack.tsx|]
    discordHelpers = mkHelpersFd discordAuthProvider [relfile|Discord.tsx|]
    gitHubHelpers = mkHelpersFd gitHubAuthProvider [relfile|GitHub.tsx|]
    googleHelpers = mkHelpersFd googleAuthProvider [relfile|Google.tsx|]
    keycloakHelpers = mkHelpersFd keycloakAuthProvider [relfile|Keycloak.tsx|]
    microsoftEntraHelpers = mkHelpersFd microsoftEntraAuthProvider [relfile|MicrosoftEntra.tsx|]

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
              "displayName" .= OAuth.displayName provider,
              "iconName" .= Data.List.filter (/= ' ') (OAuth.displayName provider)
            ]
