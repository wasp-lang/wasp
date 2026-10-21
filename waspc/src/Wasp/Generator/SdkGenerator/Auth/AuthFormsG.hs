module Wasp.Generator.SdkGenerator.Auth.AuthFormsG
  ( genAuthForms,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import StrongPath (relfile)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
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
import Wasp.Generator.SdkGenerator.Auth.Common (getOnAuthSucceededRedirectToOrDefault)
import Wasp.Generator.SdkGenerator.Common (mkTmplFdWithData)
import Wasp.Util.Js (makeJsStringLiteral)

-- | Generates the file that creates the auth form components from
--   `@wasp.sh/lib-auth` with the app's auth config.
genAuthForms :: AS.Auth.Auth -> Maybe AS.EmailSender.EmailSender -> Generator [FileDraft]
genAuthForms auth maybeEmailSender =
  return
    [ mkTmplFdWithData [relfile|auth/forms/index.ts|] tmplData
    ]
  where
    tmplData =
      object
        [ "onAuthSucceededRedirectTo" .= makeJsStringLiteral (getOnAuthSucceededRedirectToOrDefault auth),
          "isEmailAuthEnabled" .= AS.Auth.isEmailAuthEnabled auth,
          "isUsernameAndPasswordAuthEnabled" .= AS.Auth.isUsernameAndPasswordAuthEnabled auth,
          "isDummyEmailSenderNoticeEnabled" .= isDummyEmailSenderNoticeEnabled,
          "socialProviders" .= map makeSocialProviderJson enabledSocialProviders
        ]
    isDummyEmailSenderNoticeEnabled = AS.Auth.isEmailAuthEnabled auth && isDummyEmailSenderUsed
    isDummyEmailSenderUsed = maybe False AS.EmailSender.isDummyEmailSender maybeEmailSender

    -- The order decides the order of the social buttons in the forms.
    enabledSocialProviders =
      concat
        [ [slackAuthProvider | AS.Auth.isSlackAuthEnabled auth],
          [discordAuthProvider | AS.Auth.isDiscordAuthEnabled auth],
          [googleAuthProvider | AS.Auth.isGoogleAuthEnabled auth],
          [keycloakAuthProvider | AS.Auth.isKeycloakAuthEnabled auth],
          [gitHubAuthProvider | AS.Auth.isGitHubAuthEnabled auth],
          [microsoftAuthProvider | AS.Auth.isMicrosoftAuthEnabled auth]
        ]

    makeSocialProviderJson :: OAuthAuthProvider -> Aeson.Value
    makeSocialProviderJson provider =
      object
        [ "providerName" .= OAuth.providerId provider,
          "signInPath" .= OAuth.serverLoginUrl provider
        ]
