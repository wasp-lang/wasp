module Wasp.Generator.SdkGenerator.Auth.OAuthAuthG
  ( genOAuthAuth,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (File', Path', Rel', reldir, relfile, (</>))
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import Wasp.Generator.AuthProviders
  ( discordAuthProvider,
    gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
    slackAuthProvider,
  )
import Wasp.Generator.AuthProviders.OAuth (OAuthAuthProvider)
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( mkTmplFdWithDestAndData,
  )

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      genOAuthHelpers auth
  | otherwise = return []

genOAuthHelpers :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthHelpers auth =
  return $
    concat
      [ [slackHelpersFd | AS.Auth.isSlackAuthEnabled auth],
        [discordHelpersFd | AS.Auth.isDiscordAuthEnabled auth],
        [gitHubHelpersFd | AS.Auth.isGitHubAuthEnabled auth],
        [googleHelpersFd | AS.Auth.isGoogleAuthEnabled auth],
        [keycloakHelpersFd | AS.Auth.isKeycloakAuthEnabled auth]
      ]
  where
    slackHelpersFd = makeOAuthHelpersFd slackAuthProvider [relfile|Slack.tsx|]
    discordHelpersFd = makeOAuthHelpersFd discordAuthProvider [relfile|Discord.tsx|]
    gitHubHelpersFd = makeOAuthHelpersFd gitHubAuthProvider [relfile|GitHub.tsx|]
    googleHelpersFd = makeOAuthHelpersFd googleAuthProvider [relfile|Google.tsx|]
    keycloakHelpersFd = makeOAuthHelpersFd keycloakAuthProvider [relfile|Keycloak.tsx|]

    makeOAuthHelpersFd :: OAuthAuthProvider -> Path' Rel' File' -> FileDraft
    makeOAuthHelpersFd provider helpersFp =
      mkTmplFdWithDestAndData destFile tmplFile (Just tmplData)
      where
        destFile = [reldir|auth/helpers|] </> helpersFp
        tmplFile = [relfile|auth/helpers/_Provider.tsx|]
        tmplData =
          object
            [ "signInPath" .= OAuth.serverLoginUrl provider,
              "displayName" .= OAuth.displayName provider
            ]
