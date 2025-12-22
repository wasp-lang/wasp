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
import Wasp.Generator.SdkGenerator.Common

genOAuthAuth :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthAuth auth
  | AS.Auth.isExternalAuthEnabled auth =
      genOAuthHelpers auth
  | otherwise = return []

genOAuthHelpers :: AS.Auth.Auth -> Generator [FileDraft]
genOAuthHelpers auth =
  return $
    concat
      [ [slackHelpers | AS.Auth.isSlackAuthEnabled auth],
        [discordHelpers | AS.Auth.isDiscordAuthEnabled auth],
        [gitHubHelpers | AS.Auth.isGitHubAuthEnabled auth],
        [googleHelpers | AS.Auth.isGoogleAuthEnabled auth],
        [keycloakHelpers | AS.Auth.isKeycloakAuthEnabled auth]
      ]
  where
    slackHelpers = makeOAuthHelpersFds slackAuthProvider [relfile|Slack.tsx|]
    discordHelpers = makeOAuthHelpersFds discordAuthProvider [relfile|Discord.tsx|]
    gitHubHelpers = makeOAuthHelpersFds gitHubAuthProvider [relfile|GitHub.tsx|]
    googleHelpers = makeOAuthHelpersFds googleAuthProvider [relfile|Google.tsx|]
    keycloakHelpers = makeOAuthHelpersFds keycloakAuthProvider [relfile|Keycloak.tsx|]

    makeOAuthHelpersFds :: OAuthAuthProvider -> Path' Rel' File' -> FileDraft
    makeOAuthHelpersFds provider helpersFp =
      makeSdkProjectTmplFdWithDestAndData destFile SdkUserCoreProject tmplFile (Just tmplData)
      where
        destFile = [reldir|auth/helpers|] </> helpersFp
        tmplFile = [relfile|auth/helpers/_Provider.tsx|]
        tmplData =
          object
            [ "signInPath" .= OAuth.serverLoginUrl provider,
              "displayName" .= OAuth.displayName provider
            ]
