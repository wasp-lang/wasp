module Wasp.Generator.SdkGenerator.Auth.Common
  ( getOnAuthSucceededRedirectToOrDefault,
    waspAuthOptionsJson,
    waspAuthServerEnvVarNames,
    waspAuthExtensionExtImports,
  )
where

import Data.Aeson (object, (.=))
import qualified Data.Aeson.Key as Aeson.Key
import Data.Maybe (fromMaybe)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Auth.EmailVerification as AS.Auth.EmailVerification
import qualified Wasp.AppSpec.App.Auth.PasswordReset as AS.Auth.PasswordReset
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import Wasp.AppSpec.ExtImport (ExtImport)
import Wasp.AppSpec.Util (getRoutePathFromRef)
import Wasp.Generator.AuthProviders
  ( discordAuthProvider,
    gitHubAuthProvider,
    googleAuthProvider,
    keycloakAuthProvider,
    microsoftAuthProvider,
    slackAuthProvider,
  )
import qualified Wasp.Generator.AuthProviders.OAuth as OAuth
import qualified Wasp.Util.Aeson as Util.Aeson

getOnAuthSucceededRedirectToOrDefault :: AS.Auth.Auth -> String
getOnAuthSucceededRedirectToOrDefault auth = fromMaybe "/" (AS.Auth.onAuthSucceededRedirectTo auth)

-- | The serializable options the @wasp.sh/auth lib is instantiated with, on
-- both the server and the client: which methods are on and how each is
-- configured. Everything non-serializable (user functions) travels as
-- extensions instead.
waspAuthOptionsJson :: AppSpec -> AS.Auth.Auth -> String
waspAuthOptionsJson spec auth =
  Util.Aeson.encodeToString $
    object
      [ "onAuthSucceededRedirectTo" .= getOnAuthSucceededRedirectToOrDefault auth,
        "clientOAuthCallbackPath" .= OAuth.clientOAuthCallbackPath,
        "methods" .= object methodEntries
      ]
  where
    methods = AS.Auth.methods auth
    methodEntries =
      concat
        [ ["usernameAndPassword" .= object [] | AS.Auth.isUsernameAndPasswordAuthEnabled auth],
          [ "email" .= emailOptions emailConfig
          | Just emailConfig <- [AS.Auth.email methods]
          ],
          [ Aeson.Key.fromString (OAuth.providerId provider) .= object ["requiredScopes" .= OAuth.requiredScopes provider]
          | (provider, isEnabled) <- oauthProviders,
            isEnabled
          ]
        ]
    oauthProviders =
      [ (googleAuthProvider, AS.Auth.isGoogleAuthEnabled auth),
        (gitHubAuthProvider, AS.Auth.isGitHubAuthEnabled auth),
        (keycloakAuthProvider, AS.Auth.isKeycloakAuthEnabled auth),
        (slackAuthProvider, AS.Auth.isSlackAuthEnabled auth),
        (discordAuthProvider, AS.Auth.isDiscordAuthEnabled auth),
        (microsoftAuthProvider, AS.Auth.isMicrosoftAuthEnabled auth)
      ]
    emailOptions emailConfig =
      object
        [ "fromField"
            .= object
              ( ["email" .= AS.EmailSender.email fromField]
                  ++ ["name" .= name | Just name <- [AS.EmailSender.name fromField]]
              ),
          "emailVerificationClientRoute"
            .= getRoutePathFromRef spec (AS.Auth.EmailVerification.clientRoute $ AS.Auth.emailVerification emailConfig),
          "passwordResetClientRoute"
            .= getRoutePathFromRef spec (AS.Auth.PasswordReset.clientRoute $ AS.Auth.passwordReset emailConfig)
        ]
      where
        fromField = AS.Auth.fromField emailConfig

-- | The env vars Wasp's own auth reads through its adapter runtime. Its
-- compatibility privilege: these are framework-owned names, read from the
-- generated env schema rather than a manifest-declared list.
waspAuthServerEnvVarNames :: AS.Auth.Auth -> [String]
waspAuthServerEnvVarNames auth =
  concat
    [ ["JWT_SECRET"],
      onlyIf (AS.Auth.isEmailAuthEnabled auth) ["SKIP_EMAIL_VERIFICATION_IN_DEV"],
      onlyIf (AS.Auth.isGoogleAuthEnabled auth) ["GOOGLE_CLIENT_ID", "GOOGLE_CLIENT_SECRET"],
      onlyIf (AS.Auth.isGitHubAuthEnabled auth) ["GITHUB_CLIENT_ID", "GITHUB_CLIENT_SECRET"],
      onlyIf (AS.Auth.isKeycloakAuthEnabled auth) ["KEYCLOAK_CLIENT_ID", "KEYCLOAK_CLIENT_SECRET", "KEYCLOAK_REALM_URL"],
      onlyIf (AS.Auth.isSlackAuthEnabled auth) ["SLACK_CLIENT_ID", "SLACK_CLIENT_SECRET"],
      onlyIf (AS.Auth.isDiscordAuthEnabled auth) ["DISCORD_CLIENT_ID", "DISCORD_CLIENT_SECRET"],
      onlyIf (AS.Auth.isMicrosoftAuthEnabled auth) ["MICROSOFT_CLIENT_ID", "MICROSOFT_CLIENT_SECRET", "MICROSOFT_TENANT_ID"]
    ]
  where
    onlyIf cond xs = if cond then xs else []

-- | The user-authored functions Wasp's own auth calls back into, keyed by the
-- name the generated extensions module binds them under.
waspAuthExtensionExtImports :: AS.Auth.Auth -> [(String, Maybe ExtImport)]
waspAuthExtensionExtImports auth =
  [ ("userSignupFieldsUsername", AS.Auth.usernameAndPassword methods >>= AS.Auth.userSignupFieldsForUsernameAuth),
    ("userSignupFieldsEmail", AS.Auth.email methods >>= AS.Auth.userSignupFieldsForEmailAuth),
    ("userSignupFieldsGoogle", AS.Auth.google methods >>= AS.Auth.userSignupFieldsForExternalAuth),
    ("userSignupFieldsGithub", AS.Auth.gitHub methods >>= AS.Auth.userSignupFieldsForExternalAuth),
    ("userSignupFieldsKeycloak", AS.Auth.keycloak methods >>= AS.Auth.userSignupFieldsForExternalAuth),
    ("userSignupFieldsSlack", AS.Auth.slack methods >>= AS.Auth.userSignupFieldsForExternalAuth),
    ("userSignupFieldsDiscord", AS.Auth.discord methods >>= AS.Auth.userSignupFieldsForExternalAuth),
    ("userSignupFieldsMicrosoft", AS.Auth.microsoft methods >>= AS.Auth.userSignupFieldsForExternalAuth),
    ("configFnGoogle", AS.Auth.google methods >>= AS.Auth.configFn),
    ("configFnGithub", AS.Auth.gitHub methods >>= AS.Auth.configFn),
    ("configFnKeycloak", AS.Auth.keycloak methods >>= AS.Auth.configFn),
    ("configFnSlack", AS.Auth.slack methods >>= AS.Auth.configFn),
    ("configFnDiscord", AS.Auth.discord methods >>= AS.Auth.configFn),
    ("configFnMicrosoft", AS.Auth.microsoft methods >>= AS.Auth.configFn),
    ("getVerificationEmailContent", AS.Auth.email methods >>= AS.Auth.EmailVerification.getEmailContentFn . AS.Auth.emailVerification),
    ("getPasswordResetEmailContent", AS.Auth.email methods >>= AS.Auth.PasswordReset.getEmailContentFn . AS.Auth.passwordReset),
    ("onAfterEmailVerified", AS.Auth.onAfterEmailVerified auth),
    ("onBeforeOAuthRedirect", AS.Auth.onBeforeOAuthRedirect auth)
  ]
  where
    methods = AS.Auth.methods auth
